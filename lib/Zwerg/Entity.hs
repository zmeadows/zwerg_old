module Zwerg.Entity where

import Zwerg.Component
import Zwerg.Component.All
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude
import Zwerg.Util

import Control.Monad.Loops (takeWhileM)
import qualified Data.Map as M
import Data.Map.Strict (Map)

getVisibleTiles :: UUID -> MonadCompReader UUIDSet
getVisibleTiles uuid = do
  (playerX, playerY) <- unwrap <$> demandViewComp position uuid
  fov <- ceiling <$> demandViewComp viewRange uuid
  levelTiles <- demandViewComp tileMap uuid
  let minX = max (playerX - fov) 0
      minY = max (playerY - fov) 0
      maxX = min (playerX + fov) $ mapWidthINT - 1
      maxY = min (playerY + fov) $ mapHeightINT - 1
      candidatePOSs =
        M.fromList
          [ ((x, y), True)
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , sqrt ((fromIntegral x) ** 2 + (fromIntegral y) ** 2) <
              fromIntegral fov
          ]
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
  return zEmpty

tileBlocksVision :: UUID -> MonadCompReader Bool
tileBlocksVision tileUUID = return False

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

getAdjacentTileUUID :: Direction -> UUID -> MonadCompReader (Maybe UUID)
getAdjacentTileUUID dir tileUUID = do
  tilePosition <- demandViewComp position tileUUID
  let adjacentPosition = movePosDir dir tilePosition
  if | isValidPosition adjacentPosition ->
       do tileLevelUUID <- demandViewComp level tileUUID
          levelTileMap <- demandViewComp tileMap tileLevelUUID
          Just <$> tileUUIDatPosition adjacentPosition levelTileMap
     | otherwise -> return Nothing

getPrimaryOccupant :: UUID -> MonadCompReader UUID
getPrimaryOccupant tileUUID = do
  occs <- zToList <$> demandViewComp occupants tileUUID
  if (length occs) == 0
    then return tileUUID
    else do
      types <- forM occs $ \uuid -> demandViewComp entityType uuid
      let (maxUUID, _) = maximumBy (comparing snd) $ zip occs types
      return maxUUID

getOccupantsOfType :: UUID -> EntityType -> MonadCompReader UUIDSet
getOccupantsOfType containerUUID eType =
  demandViewComp occupants containerUUID >>= zFilterM isEtype
  where
    isEtype uuid = ((==) eType) <$> demandViewComp entityType uuid

removeOccupant :: UUID -> UUID -> MonadCompState ()
removeOccupant oldOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if (not $ occupiedType `elem` [Tile, Container])
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
  if (not $ occupiedType `elem` [Tile, Container])
    then throwError $
         ZError
           __FILE__
           __LINE__
           Fatal
           "Attempted to add an occupant to an entity that doesn't support it"
    else modComp occupiedUUID occupants $ zAdd newOccupantUUID

-- | TODO: remove from level, occupants, etc
eraseEntity :: UUID -> MonadCompState ()
eraseEntity uuid = do
  entityTileUUID <- readC $ getEntityTileUUID uuid
  modComp entityTileUUID occupants (zDelete uuid)
  setComp entityTileUUID needsRedraw True
  eType <- demandComp entityType uuid
  deleteComp uuid name
  deleteComp uuid glyph
  deleteComp uuid hp
  deleteComp uuid entityType
  deleteComp uuid position
  deleteComp uuid cooldown
  deleteComp uuid equipment
  deleteComp uuid baseDamage
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
  deleteComp uuid needsRedraw
