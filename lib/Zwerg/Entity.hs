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
import Zwerg.Component.All
import Zwerg.Data.Equipment
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude

import Unsafe (unsafeHead)

unEquipItem :: UUID -> EquipmentSlot -> MonadCompState ()
unEquipItem entityUUID slot = do
-- TODO add unequipped item to inventory
-- TODO deal with single/two handedness
  eqp <- equipment <@> entityUUID
  let (_, eqp') = unequip slot eqp
  setComp entityUUID equipment eqp'

equipItem :: UUID -> UUID -> MonadCompState ()
equipItem itemUUID entityUUID = do
  -- TODO lambdacase for slot/iType
  slot <- equippableSlot <@> itemUUID
  iType <- itemType <@> itemUUID
  case slot of
    Body x -> do
      unEquipItem entityUUID (Left x)
      modComp entityUUID equipment $ equip (Left x) itemUUID
    SingleHand ->
      case iType of
        Weapon -> do
          unEquipItem entityUUID (Right RightHand)
          modComp entityUUID equipment $ equip (Right RightHand) itemUUID
        Armor -> do
          unEquipItem entityUUID (Right LeftHand)
          modComp entityUUID equipment $ equip (Right LeftHand) itemUUID
        _ -> throwError $ ZError __FILE__ __LINE__ EngineFatal "invalid item type."
    BothHands -> do
      unEquipItem entityUUID (Right RightHand)
      unEquipItem entityUUID (Right LeftHand)
      modComp entityUUID equipment $ equip (Right LeftHand) itemUUID
      modComp entityUUID equipment $ equip (Right RightHand) itemUUID

getEquippedWeapon :: UUID -> MonadCompReader (Maybe UUID)
getEquippedWeapon entityUUID =
  -- TODO: handle two-handed weapons?
  getEquippedInSlot (Right RightHand) <$> equipment <~> entityUUID

getVisibleTiles :: UUID -> MonadCompReader UUIDSet
getVisibleTiles uuid = do
  -- TODO: use ST monad to implement classic impreative mutable algorithm?
  -- levelTiles <- level <~> uuid >>= demandViewComp tiles
  playerPOS <- position <~> uuid
  fov <- viewRange <~> uuid
  levelTiles <- level <~> uuid >>= demandViewComp tileMap
  let (playerX, playerY) = unwrap playerPOS
      minX, minY, maxX, maxY :: Int
      minX = round $ max (fromIntegral playerX - fov) 0.0
      minY = round $ max (fromIntegral playerY - fov) 0.0
      maxX = round $ min (fromIntegral playerX + fov) $ mapWidthDOUBLE - 1.0
      maxY = round $ min (fromIntegral playerY + fov) $ mapHeightDOUBLE - 1.0
      -- fovEdges = circle (playerX, playerY) (round $ 1.5 * fov)
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
  candidateTileUUIDs <- mapM (`tileUUIDatPosition` levelTiles) candidatePOSs
  -- return $ zFromList $ intersect candidateTileUUIDs $ concat visibleLines
  return $ zFromList candidateTileUUIDs

tileBlocksVision :: UUID -> MonadCompReader Bool
tileBlocksVision tileUUID = do
  -- The tile might iself block vision (ex: stone column)
  tileBlocks <- blocksVision <~> tileUUID
  if tileBlocks
    then return True
    else do
      -- or one of the occupants might (ex: really fat goblin)
      occs <- occupants <~> tileUUID
      -- TODO: check if 'any' rather than filter.
      occsBlock <- zFilterM (blocksVision <~>) occs
      return (zSize occsBlock > 0)

tileBlocksPassage :: UUID -> MonadCompReader Bool
tileBlocksPassage tileUUID = do
  -- The tile might itself block passage
  tileBlocks <- demandViewComp blocksPassage tileUUID
  if tileBlocks
    then return True
    else do
      -- or one the tiles occupants might block passage
      occs <- demandViewComp occupants tileUUID
      -- TODO: find first occurence rather than filter.
      occsBlock <- zFilterM (blocksPassage <~>) occs
      return (zSize occsBlock > 0)

-- DEPRECATED: use tileOn component instead
getEntityTileUUID :: UUID -> MonadCompReader UUID
getEntityTileUUID entityUUID = do
  entityLevelUUID <- demandViewComp level entityUUID
  entityPos <- demandViewComp position entityUUID
  levelTileMap <- demandViewComp tileMap entityLevelUUID
  tileUUIDatPosition entityPos levelTileMap

-- DEPRECATED: use tileOn component instead
getPlayerTileUUID :: MonadCompReader UUID
getPlayerTileUUID = do
  playerLevelUUID <- demandViewComp level playerUUID
  playerPos <- demandViewComp position playerUUID
  levelTileMap <- demandViewComp tileMap playerLevelUUID
  tileUUIDatPosition playerPos levelTileMap

getPlayerAdjacentEnemy :: Direction -> MonadCompReader (Maybe UUID)
getPlayerAdjacentEnemy dir = do
  attackedTileUUID <- getPlayerTileUUID >>= getAdjacentTileUUID dir
  case attackedTileUUID of
    Just attackedTileUUID' -> do
      adjacentTileEnemyOccupants <- getOccupantsOfType attackedTileUUID' Enemy
      if | zIsNull adjacentTileEnemyOccupants -> return Nothing
         | zSize adjacentTileEnemyOccupants == 1 ->
           return $ Just $ unsafeHead $ zToList adjacentTileEnemyOccupants
         | otherwise ->
           throwError $
           ZError __FILE__ __LINE__ EngineFatal "found multiple enemies on same tile"
    Nothing -> return Nothing

getAdjacentTileUUID :: Direction -> UUID -> MonadCompReader (Maybe UUID)
getAdjacentTileUUID dir tileUUID = do
  tilePosition <- demandViewComp position tileUUID
  case movePosDir dir tilePosition of
    Nothing -> return Nothing
    Just adjPos -> do
      tileLevelUUID <- demandViewComp level tileUUID
      levelTileMap <- demandViewComp tileMap tileLevelUUID
      Just <$> tileUUIDatPosition adjPos levelTileMap

getPrimaryOccupant :: UUID -> MonadCompReader UUID
getPrimaryOccupant occupiedUUID = do
  occs <- zToList <$> demandViewComp occupants occupiedUUID
  if null occs
    then return occupiedUUID
    else do
      types <- forM occs (demandViewComp entityType)
      -- TODO: if multiple of same max entity type, further sort somehow
      let maxUUID = fst $ maximumBy (comparing snd) $ zip occs types
      return maxUUID

getOccupantsOfType :: UUID -> EntityType -> MonadCompReader UUIDSet
getOccupantsOfType containerUUID eType =
  demandViewComp occupants containerUUID >>= zFilterM isEtype
  where
    isEtype uuid = (eType ==) <$> demandViewComp entityType uuid

removeOccupant :: UUID -> UUID -> MonadCompState ()
removeOccupant oldOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if occupiedType `notElem` [Tile, Container]
    then throwError $ ZError __FILE__ __LINE__ EngineFatal
         "Attempted to remove an occupant from an entity that doesn't support it"
    else do
      modComp occupiedUUID occupants $ zDelete oldOccupantUUID
      setComp occupiedUUID needsRedraw True

addOccupant :: UUID -> UUID -> MonadCompState ()
addOccupant newOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if occupiedType `notElem` [Tile, Container]
    then throwError $
         ZError
           __FILE__
           __LINE__
           EngineFatal
           "Attempted to add an occupant to an entity that doesn't support it"
    else do
      modComp occupiedUUID occupants $ zAdd newOccupantUUID
      setComp occupiedUUID needsRedraw True

transferOccupant :: UUID -> UUID -> UUID -> MonadCompState ()
transferOccupant transfereeUUID oldContainerUUID newContainerUUID = do
  removeOccupant transfereeUUID oldContainerUUID
  addOccupant transfereeUUID newContainerUUID
  setComp oldContainerUUID needsRedraw True
  setComp newContainerUUID needsRedraw True

-- | TODO: this is not at all complete
eraseEntity :: UUID -> MonadCompState ()
eraseEntity uuid = do
  entityTileUUID <- readC $ getEntityTileUUID uuid
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
  deleteComp uuid occupants
  deleteComp uuid parent
  deleteComp uuid children
  deleteComp uuid stats
  deleteComp uuid blocksPassage
  deleteComp uuid blocksVision
  deleteComp uuid aiType
  deleteComp uuid damageChain
  deleteComp uuid viewRange

--TODO: placeholder. Need to look at creatures Dex, etc.
resetTicks :: UUID -> MonadCompState ()
resetTicks entityUUID = setComp entityUUID ticks 50

-- getNextEntityToTick :: MonadCompReader UUID
-- getNextEntityToTick = do
--   ts <- use (ticks . uuidMap)
--   (minTick, uuids) <- getMinimumUUIDs ts
--   (ticks . uuidMap) %= fmap (\x -> max (x - minTick) 0)
--   if | notElem playerUUID uuids ->
--        forM_ uuids $ \i -> do
--          runAI i
--          processEvents
--          setComp i ticks 100
--      | otherwise -> return ()
--   return playerUUID

getTargetedUUIDs :: TargetType -> UUID -> MonadCompReader [UUID]
-- TODO: fix AOE/Line implementation (same as SingleTarget for now)
getTargetedUUIDs SingleTarget mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (AOE _) mainDefenderUUID = return [mainDefenderUUID]
getTargetedUUIDs (Line _ _) mainDefenderUUID = return [mainDefenderUUID]
