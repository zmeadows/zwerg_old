module Zwerg.Entity where

import Zwerg.Component
import Zwerg.Component.All
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude

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
  => Direction -> UUID -> m (Maybe UUID)
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
  deleteComp uuid blocked
  deleteComp uuid needsRedraw
