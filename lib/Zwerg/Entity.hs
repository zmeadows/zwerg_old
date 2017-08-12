{-|
Module      : Zwerg.Entity
Description : Generic functions that operate at the level of entities.
Copyright   : (c) Zac Meadows, 2016
License     : MIT
Maintainer  : zmeadows@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Zwerg.Entity where

import Zwerg.Component
import Zwerg.Debug
import Zwerg.Data.GridMap
import Zwerg.Data.Equipment
import Zwerg.Data.Position
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Geometry.FOV
import Zwerg.Prelude
import Zwerg.Util

import Control.Monad.Loops (anyM)

{-
addToInventory :: UUID -> UUID -> MonadCompState ()
addToInventory itemUUID holderUUID = do
  -- parent <@> itemUUID >>= \case
  --   Alive itemParentUUID -> modComp itemParentUUID inventory $ zDelete itemUUID
  --   _ -> return ()

  zAdd itemUUID <$> modComp holderUUID inventory
  setComp itemUUID parent $ Alive holderUUID
  (tileOn, position, zLevel) <@@===> (holderUUID, itemUUID)
-}

getVisionBlockedTiles :: UUID -> MonadCompRead (GridMap Bool)
getVisionBlockedTiles levelUUID = do
    whenM ((/= Level) <$> entityType <~> levelUUID) $
        debug "Tried to compute vision-blocked tile map for entity that isn't a Level."
    tm <- tileMap <~> levelUUID
    zBuildM $ \pos -> tileBlocksVision (zAt tm pos)
  where tileBlocksVision :: UUID -> MonadCompRead Bool
        tileBlocksVision tileUUID = do
          tileBlocks <- blocksVision <~> tileUUID
          if tileBlocks
            then return True
            else do
              occs <- occupants <~> tileUUID
              anyM (blocksVision <~>) (unwrap occs)

getVisibleTiles :: UUID -> MonadCompRead [UUID]
getVisibleTiles uuid = do
    (playerPOS, fov, levelUUID) <- (position, viewRange, level) <~!!!> uuid
    levelBlockedTiles <- getVisionBlockedTiles levelUUID
    let visibleTiles = getFOV playerPOS fov levelBlockedTiles
    levelTileMap <- tileMap <~> levelUUID
    return $ map (zAt levelTileMap) visibleTiles

getItemsOnEntityTile :: UUID -> MonadCompRead UUIDSet
getItemsOnEntityTile entityUUID = tileOn <~> entityUUID >>= (`getOccupantsOfType` Item)

unequipItem :: UUID -> EquipmentSlot -> MonadCompState ()
unequipItem entityUUID islot = do
-- TODO add unequipped item to inventory
  (_, newEquipment) <- unequip islot <$> equipment <@> entityUUID
  setComp entityUUID equipment newEquipment

equipItem :: UUID -> UUID -> MonadCompState ()
equipItem itemUUID entityUUID = do
  islot <- slot <@> itemUUID
  -- TODO add unequipped item to inventory
  -- TODO check that itemUUID has correct type
  -- TODO set parent of item
  (_, newEquipment) <- equip islot itemUUID <$> equipment <@> entityUUID
  setComp entityUUID equipment newEquipment

getEquippedWeapon :: UUID -> MonadCompRead (Maybe UUID)
getEquippedWeapon entityUUID = do
  uuids <- getEquippedInSlot (SingleHand RightHand) <$> equipment <~> entityUUID
  filterM (isItemType Weapon) uuids >>= \case
    [] -> return Nothing
    [x] -> return $ Just x
    xs -> do
        debug "Entity has multiple weapons equipped, and dual-wielding is not yet implemented."
        return $ Just $ head xs

isItemType :: ItemType -> UUID -> MonadCompRead Bool
isItemType itypetest uuid = entityType <~> uuid >>= \case
    Item -> (== itypetest) <$> itemType <~> uuid
    _ -> debug "attempted to compare ItemType for non-Item entity." *> return False


tileBlocksPassage :: UUID -> MonadCompRead Bool
tileBlocksPassage tileUUID = do
  -- The tile might itself block passage
  tileBlocks <- blocksPassage <~> tileUUID
  if tileBlocks
    then return True
    else do
      -- or one the tiles occupants might block passage
      occs <- occupants <~> tileUUID
      anyM (blocksPassage <~>) (unwrap occs)

getAdjacentTileUUID :: Direction -> UUID -> MonadCompRead (Maybe UUID)
getAdjacentTileUUID dir tileUUID = do
  tilePosition <- position <~> tileUUID
  case movePosDir dir tilePosition of
    Nothing -> return Nothing
    Just adjPos -> do
      tileLevelUUID <- level <~> tileUUID
      levelTileMap <- tileMap <~> tileLevelUUID
      return $ Just $ zAt levelTileMap adjPos

getOccupantsOfType :: UUID -> EntityType -> MonadCompRead UUIDSet
getOccupantsOfType containerUUID eType = occupants <~> containerUUID >>= zFilterM isEtype
  where isEtype uuid = (eType ==) <$> entityType <~> uuid

transferOccupant :: UUID -> Maybe UUID -> UUID -> MonadCompState ()
transferOccupant transfereeUUID oldContainerUUID newContainerUUID =
  let addOccupant = do
        occupiedType <- entityType <@> newContainerUUID
        if occupiedType `notElem` [Tile, Container]
          then debug "Attempted to add an occupant to an entity that doesn't support it"
          else do
            --TODO: change z-level hasn't changed?
            modComp newContainerUUID occupants $ zAdd transfereeUUID
            position <@> newContainerUUID >>= setComp transfereeUUID position
            when (occupiedType == Tile) $ do
               setComp newContainerUUID needsRedraw True
               setComp transfereeUUID tileOn newContainerUUID
      removeOccupant oldContainerUUID' = do
        occupiedType <- entityType <@> oldContainerUUID'
        if occupiedType `notElem` [Tile, Container]
          then debug "Attempted to remove an occupant from an entity that doesn't support it"
          else do
            modComp oldContainerUUID' occupants $ zDelete transfereeUUID
            when (occupiedType == Tile) $ setComp oldContainerUUID' needsRedraw True
  in whenJust oldContainerUUID removeOccupant >> addOccupant

-- | TODO: this is not at all complete
eraseEntity :: UUID -> MonadCompState ()
eraseEntity uuid = do
  -- TODO: not all entities are on a tile?
  entityTileUUID <- tileOn <@> uuid
  modComp entityTileUUID occupants (zDelete uuid)
  setComp entityTileUUID needsRedraw True

  deleteComp uuid name
  deleteComp uuid glyph
  deleteComp uuid hp
  deleteComp uuid entityType
  deleteComp uuid position
  deleteComp uuid cooldown
  deleteComp uuid equipment
  deleteComp uuid level
  deleteComp uuid tiles
  deleteComp uuid ticks
  deleteComp uuid tileType
  deleteComp uuid tileMap
  deleteComp uuid occupants
  deleteComp uuid parent
  deleteComp uuid children
  deleteComp uuid stats
  deleteComp uuid blocksPassage
  deleteComp uuid blocksVision
  deleteComp uuid aiType
  deleteComp uuid damageChain
  deleteComp uuid viewRange
  deleteComp uuid slot
  deleteComp uuid itemType
  deleteComp uuid needsRedraw
  deleteComp uuid zLevel

--TODO: placeholder. Need to look at creatures DEX, etc
--TODO: define maximum Stat value
resetTicks :: UUID -> MonadCompState ()
resetTicks uuid = setComp uuid ticks 100
  -- dex <- lookupStat DEX <$> stats <~> uuid
  -- return 100 * (1.0 - (fromIntegral dex) / 200.0)

getTargetedUUIDs :: TargetType -> UUID -> MonadCompRead [UUID]
-- TODO: fix AOE/Line implementation (same as SingleTarget for now)
getTargetedUUIDs SingleTarget mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (AOE _) mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (Line _ _) mainDefenderUUID = return [mainDefenderUUID]

getFearLevel :: UUID -> MonadCompRead Text
getFearLevel _ = return "Terrifying"

--TODO: make Stats instance of ZMapContainer
getStat :: Stat -> UUID -> MonadCompRead Int
getStat someStat entityUUID = lookupStat someStat <$> stats <~> entityUUID
