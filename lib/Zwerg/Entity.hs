{-|
Module      : Zwerg.Entity
Description : Generic functions that operate at the level of entities.
Copyright   : (c) Zac Meadows, 2016
License     : GPL-3
Maintainer  : zmeadows@gmail.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Zwerg.Entity where

import Zwerg.Component
import Zwerg.Data.Equipment
import Zwerg.Data.Position
import Zwerg.Data.GridMap
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude
import Zwerg.Util

import Control.Monad.Loops (anyM)

{-# INLINABLE getItemsOnEntityTile #-}
getItemsOnEntityTile :: UUID -> MonadCompReader UUIDSet
getItemsOnEntityTile entityUUID = tileOn <~> entityUUID >>= (`getOccupantsOfType` Item)

{-# INLINABLE unequipItem #-}
unequipItem :: UUID -> EquipmentSlot -> MonadCompState ()
unequipItem entityUUID islot = do
-- TODO add unequipped item to inventory
  (_, newEquipment) <- unequip islot <$> equipment <@> entityUUID
  setComp entityUUID equipment newEquipment

{-# INLINABLE equipItem #-}
equipItem :: UUID -> UUID -> MonadCompState ()
equipItem itemUUID entityUUID = do
  islot <- slot <@> itemUUID
  -- TODO add unequipped item to inventory
  (_, newEquipment) <- equip islot itemUUID <$> equipment <@> entityUUID
  setComp entityUUID equipment newEquipment

{-# INLINABLE getEquippedWeapon #-}
getEquippedWeapon :: UUID -> MonadCompReader (Maybe UUID)
getEquippedWeapon entityUUID = do
  uuids <- getEquippedInSlot (SingleHand RightHand) <$> equipment <~> entityUUID
  filterM (isItemType Weapon) uuids >>= \case
    [] -> return Nothing
    [x] -> return $ Just x
    _ -> $(throw) EngineFatal "Entity has multiple weapons equipped, and dual-wielding is not yet implemented."

{-# INLINABLE isItemType #-}
isItemType :: ItemType -> UUID -> MonadCompReader Bool
isItemType itypetest uuid =
  entityType <~> uuid >>= \case
    Item -> do
      itype <- itemType <~> uuid
      return (itype == itypetest)
    _ -> $(throw) EngineFatal "attempted to compare ItemType for non-Item entity."

getVisibleTiles :: UUID -> MonadCompReader UUIDSet
getVisibleTiles uuid = do
  -- TODO: use ST monad to implement classic impreative mutable algorithm?
  -- levelTiles <- level <~> uuid >>= (<~>) tiles
  playerPOS <- position <~> uuid
  fov <- viewRange <~> uuid
  levelTiles <- level <~> uuid >>= (<~>) tileMap
  let (playerX, playerY) = unwrap playerPOS
      minX, minY, maxX, maxY :: Int
      minX = round $ max (fromIntegral playerX - fov) 0.0
      minY = round $ max (fromIntegral playerY - fov) 0.0
      maxX = round $ min (fromIntegral playerX + fov) $ mapWidthDOUBLE - 1.0
      maxY = round $ min (fromIntegral playerY + fov) $ mapHeightDOUBLE - 1.0
      -- fovEdges =
      --   filter (\(x,y) -> x >= minX && x <= maxX && y >= minY && y <= maxY)
      --   $ circle (playerX, playerY) (round $ 1.4 * fov)
      -- linesToFovEdges = map (unsafeTail . (line (playerX, playerY))) fovEdges
  -- tileUUIDlinesToFovEdges <-
  --   (mapM . mapM)
  --     (zConstruct >=> flip tileUUIDatPosition levelTiles)
  --     linesToFovEdges
  -- visibleLines <-
  --   forM tileUUIDlinesToFovEdges $
  --   takeWhileM1 (tileBlocksPassage >=> return . not)
  candidatePOSs <-
    filter (\p -> distance Euclidean playerPOS p < fov) <$>
    mapM zConstruct [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  let candidateTileUUIDs = map (`atPos` levelTiles) candidatePOSs
  -- return $ zFromList $ intersect candidateTileUUIDs $ concat visibleLines
  return $ zFromList candidateTileUUIDs

{-# INLINABLE tileBlocksVision #-}
tileBlocksVision :: UUID -> MonadCompReader Bool
tileBlocksVision tileUUID = do
  -- The tile might iself block vision (ex: stone column)
  tileBlocks <- blocksVision <~> tileUUID
  if tileBlocks
    then return True
    else do
      -- or one of the occupants might (ex: really fat goblin)
      occs <- occupants <~> tileUUID
      anyM (blocksVision <~>) (unwrap occs)

{-# INLINABLE tileBlocksPassage #-}
tileBlocksPassage :: UUID -> MonadCompReader Bool
tileBlocksPassage tileUUID = do
  -- The tile might itself block passage
  tileBlocks <- blocksPassage <~> tileUUID
  if tileBlocks
    then return True
    else do
      -- or one the tiles occupants might block passage
      occs <- occupants <~> tileUUID
      anyM (blocksPassage <~>) (unwrap occs)

getPlayerAdjacentEnemy :: Direction -> MonadCompReader (Maybe UUID)
getPlayerAdjacentEnemy dir = do
  attackedTileUUID <- tileOn <~> playerUUID >>= getAdjacentTileUUID dir
  case attackedTileUUID of
    Just attackedTileUUID' -> do
      zToList <$> getOccupantsOfType attackedTileUUID' Enemy >>= \case
        [] -> return Nothing
        [x] -> return $ Just x
        _ -> $(throw) EngineFatal "found multiple enemies on same tile"
    Nothing -> return Nothing

getAdjacentTileUUID :: Direction -> UUID -> MonadCompReader (Maybe UUID)
getAdjacentTileUUID dir tileUUID = do
  tilePosition <- position <~> tileUUID
  case movePosDir dir tilePosition of
    Nothing -> return Nothing
    Just adjPos -> do
      tileLevelUUID <- level <~> tileUUID
      levelTileMap <- tileMap <~> tileLevelUUID
      return $ Just $ atPos adjPos levelTileMap

getPrimaryOccupant :: UUID -> MonadCompReader UUID
getPrimaryOccupant occupiedUUID = do
  occs <- zToList <$> occupants <~> occupiedUUID
  if null occs
    then return occupiedUUID
    else do
      types <- forM occs ((<~>) entityType)
      -- TODO: if multiple of same max entity type, further sort somehow
      -- TODO: factor out a 'sortOccupantsByViewPriority' function?
      let maxUUID = fst $ maximumBy (comparing snd) $ zip occs types
      return maxUUID

{-# INLINABLE getOccupantsOfType #-}
getOccupantsOfType :: UUID -> EntityType -> MonadCompReader UUIDSet
getOccupantsOfType containerUUID eType = occupants <~> containerUUID >>= zFilterM isEtype
  where isEtype uuid = (eType ==) <$> entityType <~> uuid

transferOccupant :: UUID -> (Maybe UUID) -> UUID -> MonadCompState ()
transferOccupant transfereeUUID oldContainerUUID newContainerUUID =
  let addOccupant = do
        occupiedType <- entityType <@> newContainerUUID
        if occupiedType `notElem` [Tile, Container]
          then $(throw) EngineFatal "Attempted to add an occupant to an entity that doesn't support it"
          else do
            --TODO: check z-level hasn't changed?
            modComp newContainerUUID occupants $ zAdd transfereeUUID
            position <@> newContainerUUID >>= setComp transfereeUUID position
            when (occupiedType == Tile) $ do
               setComp newContainerUUID needsRedraw True
               setComp transfereeUUID tileOn newContainerUUID
      removeOccupant oldContainerUUID' = do
        occupiedType <- entityType <@> oldContainerUUID'
        if occupiedType `notElem` [Tile, Container]
          then $(throw) EngineFatal "Attempted to remove an occupant from an entity that doesn't support it"
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

--TODO: placeholder. Need to look at creatures Dex, etc
--TODO: define maximum Statu value
resetTicks :: UUID -> MonadCompState ()
resetTicks uuid = setComp uuid ticks 100
  -- dex <- lookupStat DEX <$> stats <~> uuid
  -- return 100 * (1.0 - (fromIntegral dex) / 200.0)

getTargetedUUIDs :: TargetType -> UUID -> MonadCompReader [UUID]
-- TODO: fix AOE/Line implementation (same as SingleTarget for now)
getTargetedUUIDs SingleTarget mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (AOE _) mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (Line _ _) mainDefenderUUID = return [mainDefenderUUID]

getFearLevel :: UUID -> MonadCompReader Text
getFearLevel _ = return "Terrifying"
