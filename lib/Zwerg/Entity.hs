module Zwerg.Entity where

import Zwerg.Class
import Zwerg.Component
import Zwerg.Component.All
import qualified Zwerg.Data.Direction as Dir
import Zwerg.Data.Error
import Zwerg.Data.UUIDMap
import Zwerg.Data.UUIDSet (UUIDSet)
import qualified Zwerg.Data.UUIDSet as US
import Zwerg.Prelude

import Control.Lens ((%=))

getEntityTileUUID
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => UUID -> m UUID
getEntityTileUUID entityUUID = do
  entityLevelUUID <- demandComp level entityUUID
  entityPos <- demandComp position entityUUID
  levelTileMap <- demandComp tileMap entityLevelUUID
  tileUUIDatPosition entityPos levelTileMap

getPlayerTileUUID
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => m UUID
getPlayerTileUUID = do
  playerLevelUUID <- demandComp level playerUUID
  playerPos <- demandComp position playerUUID
  levelTileMap <- demandComp tileMap playerLevelUUID
  tileUUIDatPosition playerPos levelTileMap

getAdjacentTileUUID
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => Dir.Direction -> UUID -> m (Maybe UUID)
getAdjacentTileUUID dir tileUUID = do
  tilePosition <- demandComp position tileUUID
  let adjacentPosition = movePosDir dir tilePosition
  if | isValidPosition adjacentPosition ->
       do tileLevelUUID <- demandComp level tileUUID
          levelTileMap <- demandComp tileMap tileLevelUUID
          Just <$> tileUUIDatPosition adjacentPosition levelTileMap
     | otherwise -> return Nothing

getPrimaryOccupant
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => UUID -> m UUID
getPrimaryOccupant tileUUID = do
  demandHasComp tileUUID tileType
  occs <- zToList <$> demandComp occupants tileUUID
  if (length occs) == 0
    then return tileUUID
    else do
      types <- forM occs $ \uuid -> demandComp entityType uuid
      let (maxUUID, _) = maximumBy (comparing snd) $ zip occs types
      return maxUUID

getTileOccupantsOfType
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => UUID -> EntityType -> m UUIDSet
getTileOccupantsOfType tileUUID et =
  demandComp occupants tileUUID >>= zFilterM isEnemy
  where
    isEnemy uuid = demandComp entityType uuid >>= pure . ((==) et)

removeOccupant
  :: (HasComponents s, MonadState s m, MonadError ZError m)
  => UUID -> UUID -> m ()
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

addOccupant
  :: (HasComponents s, MonadState s m, MonadError ZError m)
  => UUID -> UUID -> m ()
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
eraseEntity
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => UUID -> m ()
eraseEntity uuid = do
  entityTileUUID <- getEntityTileUUID uuid
  modComp entityTileUUID occupants (zDelete uuid)
  setComp entityTileUUID needsRedraw True
  eType <- demandComp entityType uuid
  when (eType == Enemy) $ setComp entityTileUUID blocked False
  (name . uuidMap) %= zRemoveAt uuid
  (glyph . uuidMap) %= zRemoveAt uuid
  (hp . uuidMap) %= zRemoveAt uuid
  (entityType . uuidMap) %= zRemoveAt uuid
  (position . uuidMap) %= zRemoveAt uuid
  (cooldown . uuidMap) %= zRemoveAt uuid
  (equipment . uuidMap) %= zRemoveAt uuid
  (baseDamage . uuidMap) %= zRemoveAt uuid
  (level . uuidMap) %= zRemoveAt uuid
  (tiles . uuidMap) %= zRemoveAt uuid
  (ticks . uuidMap) %= zRemoveAt uuid
  (tileType . uuidMap) %= zRemoveAt uuid
  (occupants . uuidMap) %= zRemoveAt uuid
  (parent . uuidMap) %= zRemoveAt uuid
  (children . uuidMap) %= zRemoveAt uuid
  (stats . uuidMap) %= zRemoveAt uuid
  (blocked . uuidMap) %= zRemoveAt uuid
  (needsRedraw . uuidMap) %= zRemoveAt uuid
