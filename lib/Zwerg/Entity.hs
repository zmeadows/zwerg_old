{-|
Module      : Zwerg.Entity
Description : Functions that operate at the level of entities.
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
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude

-- import Zwerg.Util
-- import Control.Monad.Loops (takeWhileM)
-- import qualified Data.Map as M
-- import Data.Map.Strict (Map)
import Unsafe (unsafeHead)

unEquipItem :: UUID -> EquipmentSlot -> MonadCompState ()
unEquipItem entityUUID slot = do
  eqp <- demandComp equipment entityUUID
  let (_, eqp') = unequip slot eqp
  setComp entityUUID equipment eqp'
  -- TODO: add unequipped item to inventory
  -- TODO: deal with single/two handedness

equipItem :: UUID -> UUID -> MonadCompState ()
equipItem itemUUID entityUUID = do
  slot <- demandComp equippableSlot itemUUID
  iType <- demandComp itemType itemUUID
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
        _ -> throwError $ ZError __FILE__ __LINE__ Fatal "invalid item type."
    BothHands -> do
      unEquipItem entityUUID (Right RightHand)
      unEquipItem entityUUID (Right LeftHand)
      modComp entityUUID equipment $ equip (Right LeftHand) itemUUID
      modComp entityUUID equipment $ equip (Right RightHand) itemUUID

getVisibleTiles :: UUID -> MonadCompReader UUIDSet
getVisibleTiles uuid = do
  playerPOS <- demandViewComp position uuid
  fov <- demandViewComp viewRange uuid
  levelUUID <- demandViewComp level uuid
  levelTiles <- demandViewComp tileMap levelUUID
  let (playerX, playerY) = unwrap playerPOS
      minX, minY, maxX, maxY :: Int
      minX = round $ max (fromIntegral playerX - fov) 0.0
      minY = round $ max (fromIntegral playerY - fov) 0.0
      maxX = round $ min (fromIntegral playerX + fov) $ mapWidthDOUBLE - 1.0
      maxY = round $ min (fromIntegral playerY + fov) $ mapHeightDOUBLE - 1.0
  -- traceShowM (minX, maxX, minX, maxY)
  candidatePOSs <-
    filter (\p -> distance Euclidean playerPOS p < fov) <$>
    mapM zConstruct [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  zFromList <$> mapM (`tileUUIDatPosition` levelTiles) candidatePOSs
      -- fovEdges =
      --   map (minX, ) [minY + 1 .. maxY - 1] ++
      --   map (maxX, ) [minY + 1 .. maxY - 1] ++
      --   map (, minY) [minY .. maxY] ++ map (, maxY) [minY .. maxY]
      -- linesToFovEdges = map (line (playerX, playerY)) fovEdges
  -- visibleLines <-
  --   forM linesToFovEdges $ \sightLine -> do
  --     (flip takeWhileM) sightLine $ \intPair -> return True
  -- let markTiles [] candidates = return candidates
  --     markTiles (l:ls) candidates = do
  --       let modifiedCandidates = M.adjust (\k _ -> True) candidates
  --       markTiles ls candidates

tileBlocksVision :: UUID -> MonadCompReader Bool
tileBlocksVision tileUUID = do
  tileBlocks <- demandViewComp blocksVision tileUUID
  if tileBlocks
    then return True
    else do
      occs <- demandViewComp occupants tileUUID
      -- TODO: find first occurence rather than filter.
      occsBlock <- zFilterM (demandViewComp blocksVision) occs
      return (zSize occsBlock > 0)

tileBlocksPassage :: UUID -> MonadCompReader Bool
tileBlocksPassage tileUUID = do
  tileBlocks <- demandViewComp blocksPassage tileUUID
  if tileBlocks
    then return True
    else do
      occs <- demandViewComp occupants tileUUID
      -- TODO: find first occurence rather than filter.
      occsBlock <- zFilterM (demandViewComp blocksPassage) occs
      return (zSize occsBlock > 0)

getEntityTileUUID :: UUID -> MonadCompReader UUID
getEntityTileUUID entityUUID = do
  entityLevelUUID <- demandViewComp level entityUUID
  entityPos <- demandViewComp position entityUUID
  levelTileMap <- demandViewComp tileMap entityLevelUUID
  tileUUIDatPosition entityPos levelTileMap

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
         | zSize adjacentTileEnemyOccupants > 1 ->
           throwError $
           ZError __FILE__ __LINE__ Fatal "found multiple enemies on same tile"
         | otherwise ->
           return $ Just $ unsafeHead $ zToList adjacentTileEnemyOccupants
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
    then throwError $
         ZError
           __FILE__
           __LINE__
           Fatal
           "Attempted to remove an occupant from an entity that doesn't support it"
    else modComp occupiedUUID occupants $ zDelete oldOccupantUUID

addOccupant :: UUID -> UUID -> MonadCompState ()
addOccupant newOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if occupiedType `notElem` [Tile, Container]
    then throwError $
         ZError
           __FILE__
           __LINE__
           Fatal
           "Attempted to add an occupant to an entity that doesn't support it"
    else modComp occupiedUUID occupants $ zAdd newOccupantUUID

transferOccupant :: UUID -> UUID -> UUID -> MonadCompState ()
transferOccupant transfereeUUID oldContainerUUID newContainerUUID = do
  removeOccupant transfereeUUID oldContainerUUID
  addOccupant transfereeUUID newContainerUUID

-- | TODO: this is not at all complete
eraseEntity :: UUID -> MonadCompState ()
eraseEntity uuid = do
  entityTileUUID <- readC $ getEntityTileUUID uuid
  modComp entityTileUUID occupants (zDelete uuid)
  deleteComp uuid name
  deleteComp uuid glyph
  deleteComp uuid hp
  deleteComp uuid entityType
  deleteComp uuid position
  deleteComp uuid cooldown
  deleteComp uuid equipment
  deleteComp uuid level
  deleteComp uuid tileMap
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
