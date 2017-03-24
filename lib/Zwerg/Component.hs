module Zwerg.Component where

import Zwerg.Prelude
import Zwerg.Class
import Zwerg.Component.All
import Zwerg.Data.UUIDMap (UUIDMap)
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Data.Error
import qualified Zwerg.Data.Direction as Direction
import Zwerg.UI.GlyphMap
import Zwerg.Util
import Zwerg.Const
import qualified Zwerg.Component.TileMap as TM

import Data.Binary
import Data.Foldable (maximumBy)
import Data.Text (Text)
import Data.Ord (comparing)
import Control.Exception.Base (assert)
import GHC.Generics (Generic)

import Control.Lens (makeClassy, Lens', at, use, to, (%=))

data Components = Components
    { _name        :: UUIDMap Text
    , _glyph       :: UUIDMap Glyph
    , _hp          :: UUIDMap HP
    , _entityType  :: UUIDMap EntityType
    , _position    :: UUIDMap Position
    , _cooldown    :: UUIDMap Int
    , _equipment   :: UUIDMap Equipment
    , _baseDamage  :: UUIDMap Int
    , _level       :: UUIDMap UUID
    , _tileMap     :: UUIDMap TileMap
    , _tiles       :: UUIDMap UUIDSet
    , _tileType    :: UUIDMap TileType
    , _occupants   :: UUIDMap UUIDSet
    , _parent      :: UUIDMap Parent
    , _children    :: UUIDMap UUIDSet
    , _stats       :: UUIDMap Stats
    , _blocked     :: UUIDMap Bool
    , _needsRedraw :: UUIDMap Bool
    } deriving (Show, Eq, Generic)
makeClassy ''Components

instance Binary Components

type Component s a = HasComponents s => Lens' s (UUIDMap a)

emptyComponents :: Components
emptyComponents = Components
  zEmpty zEmpty zEmpty zEmpty zEmpty zEmpty zEmpty
  zEmpty zEmpty zEmpty zEmpty zEmpty zEmpty zEmpty
  zEmpty zEmpty zEmpty zEmpty 


getComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> m (Maybe a)
getComp uuid comp = use (comp . at uuid)

hasComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> m Bool
hasComp uuid comp = use $ comp . to (zContains uuid)

addComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> a
        -> m ()
addComp uuid comp dat = comp %= zInsert uuid dat

setComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> a
        -> m ()
setComp = addComp

modComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> (a -> a)
        -> m ()
modComp uuid comp f = comp %= zAdjust f uuid

filterComp :: (HasComponents s, MonadState s m)
           => Component s a
           -> (a -> Bool)
           -> m ()
filterComp comp f = comp %= zFilter (\(_,x) -> f x)

eraseEntity :: (HasComponents s, MonadState s m)
            => UUID
            -> m ()
eraseEntity uuid =
  let eraseUUID :: UUIDMap a -> UUIDMap a
      eraseUUID = zRemoveAt uuid
  in do
    name        %= eraseUUID
    glyph       %= eraseUUID
    hp          %= eraseUUID
    entityType  %= eraseUUID
    position    %= eraseUUID
    cooldown    %= eraseUUID
    equipment   %= eraseUUID
    baseDamage  %= eraseUUID
    level       %= eraseUUID
    tiles       %= eraseUUID
    tileType    %= eraseUUID
    occupants   %= eraseUUID
    parent      %= eraseUUID
    children    %= eraseUUID
    stats       %= eraseUUID
    blocked     %= eraseUUID
    needsRedraw %= eraseUUID

demandComp :: ( HasComponents s,
                MonadError ZError m,
                MonadState s m
              )
           => Component s a
           -> UUID
           -> m a
demandComp comp uuid = do
    result <- getComp uuid comp
    whenJustErr result (ZError __FILE__ __LINE__ Fatal "Missing Component") return

demandHasComp :: (HasComponents s, MonadState s m)
          => UUID
          -> Component s a
          -> m ()
demandHasComp uuid comp = do
    result <- hasComp uuid comp
    assert result $ return ()

addOccupant :: (HasComponents s, MonadState s m, MonadError ZError m)
            => UUID -> UUID -> m ()
addOccupant newOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if (not $ occupiedType `elem` [Tile, Container])
  then throwError $ ZError __FILE__ __LINE__ Fatal
                    "Attempted to add an occupant to an entity that doesn't support it"
  else modComp occupiedUUID occupants $ zAdd newOccupantUUID

removeOccupant :: (HasComponents s, MonadState s m, MonadError ZError m)
               => UUID -> UUID -> m ()
removeOccupant oldOccupantUUID occupiedUUID = do
  occupiedType <- demandComp entityType occupiedUUID
  if (not $ occupiedType `elem` [Tile, Container])
  then throwError $ ZError __FILE__ __LINE__ Fatal
                    "Attempted to remove an occupant from an entity that doesn't support it"
  else modComp occupiedUUID occupants $ zDelete oldOccupantUUID

getPrimaryOccupant :: ( HasComponents s,
                        MonadError ZError m,
                        MonadState s m
                      )
                   => UUID -> m UUID
getPrimaryOccupant tileUUID = do
    demandHasComp tileUUID tileType
    occs <- zToList <$> demandComp occupants tileUUID
    if (length occs) == 0
      then return tileUUID
      else do
        types <- forM occs $ \uuid -> demandComp entityType uuid
        let (maxUUID,_) = maximumBy (comparing snd) $ zip occs types
        return maxUUID

getGlyphMapUpdates :: ( HasComponents s
                      ,  MonadError ZError m
                      ,  MonadState s m
                      )
                   => m GlyphMap
getGlyphMapUpdates = do
  currentLevelUUID <- demandComp level playerUUID
  currentLevelTiles <- demandComp tiles currentLevelUUID
  tilesWithUpdatedNeeded <- zFilterM (demandComp needsRedraw) currentLevelTiles
  updatedGlyphs <- forM (zToList tilesWithUpdatedNeeded) $ \tileUUID -> do
    pos <- demandComp position tileUUID
    occUUID <- getPrimaryOccupant tileUUID
    gly <- demandComp glyph occUUID
    setComp tileUUID needsRedraw False
    return (pos, gly)
  return $ mkGlyphMap updatedGlyphs

moveEntityDirection :: ( HasComponents s
                       , MonadError ZError m
                       , MonadState s m
                       )
                    => UUID
                    -> Direction.Direction
                    -> m ()
moveEntityDirection uuid dir = do
  (x,y) <- unPosition <$> demandComp position uuid
  let (x',y') = if
        | dir == Direction.Left  -> (x-1,y)
        | dir == Direction.Right -> (x+1,y)
        | dir == Direction.Up    -> (x,y-1)
        | dir == Direction.Down  -> (x,y+1)

  let destinationOutsideMap = (x' < 0 || x' >= round mapWidth || y' < 0 || y' >= round mapHeight)

  when (destinationOutsideMap && uuid /= playerUUID) $
    throwError $ ZError __FILE__ __LINE__ Fatal
                 "NPC Entity attempted to move outside of map"

  when (destinationOutsideMap && uuid == playerUUID) $
    throwError $ ZError __FILE__ __LINE__ Warning
                 "Player attempted to move outside of map"

  let oldPosition = mkPosition (x,y)
      newPosition = mkPosition (x',y')
    
  levelUUID      <- demandComp level uuid
  levelTiles     <- demandComp tileMap levelUUID
  newTileUUID    <- TM.tileUUIDatPosition newPosition levelTiles
  newTileBlocked <- demandComp blocked newTileUUID

  when (newTileBlocked && uuid /= playerUUID) $
    throwError $ ZError __FILE__ __LINE__ Fatal
                 "NPC Entity attempted to move to blocked tile"

  when (newTileBlocked && uuid == playerUUID) $
    throwError $ ZError __FILE__ __LINE__ Warning
                 "Player attempted to move to a blocked tile"

  oldTileUUID <- TM.tileUUIDatPosition oldPosition levelTiles

  setComp uuid position newPosition

  removeOccupant uuid oldTileUUID
  setComp oldTileUUID blocked False

  addOccupant uuid newTileUUID
  setComp newTileUUID blocked True

  setComp oldTileUUID needsRedraw True
  setComp newTileUUID needsRedraw True

