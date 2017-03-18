module Zwerg.Component where

import Zwerg.Component.All

import Zwerg.Data.UUIDSet
import Zwerg.Data.UUIDMap (UUIDMap)
import qualified Zwerg.Data.UUIDMap as UM

import Data.Binary
import Data.Foldable (maximumBy)
import Data.Text (Text)
import Data.Ord (comparing)
import Data.Maybe (isJust, fromJust)
import Control.Monad.State.Strict (MonadState, forM)
import Control.Exception.Base (assert)
import GHC.Generics (Generic)

import Control.Lens (
    makeClassy,
    Lens',
    at, use, to,
    (%=)
    )

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
    , _tiles       :: UUIDMap Tiles
    , _tileType    :: UUIDMap TileType
    , _occupants   :: UUIDMap UUIDSet
    , _parent      :: UUIDMap Parent
    , _children    :: UUIDMap UUIDSet
    , _stats       :: UUIDMap Stats
    , _blocked     :: UUIDMap Bool
    , _needsRedraw :: UUIDMap Bool
    , _playerUUID  :: UUID
    } deriving (Show, Read, Eq, Generic)
makeClassy ''Components

instance Binary Components

type Component s a = HasComponents s => Lens' s (UUIDMap a)

emptyComponents :: Components
emptyComponents = Components UM.empty UM.empty UM.empty UM.empty UM.empty UM.empty
                             UM.empty UM.empty UM.empty UM.empty UM.empty UM.empty
                             UM.empty UM.empty UM.empty UM.empty UM.empty (mkUUID 0)

getComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> m (Maybe a)
getComp uuid comp = use (comp . at uuid)

hasComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> m Bool
hasComp uuid comp = use $ comp . to (UM.member uuid)

addComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> a
        -> m ()
addComp uuid comp dat = comp %= UM.insert uuid dat

modComp :: (HasComponents s, MonadState s m)
        => UUID
        -> Component s a
        -> (a -> a)
        -> m ()
modComp uuid comp f = comp %= UM.adjust f uuid

filterComp :: (HasComponents s, MonadState s m)
           => Component s a
           -> (a -> Bool)
           -> m ()
filterComp comp f = comp %= UM.filter f

eraseEntityComps :: (HasComponents s, MonadState s m)
            => UUID
            -> m ()
eraseEntityComps uuid =
  let eraseUUID :: UUIDMap a -> UUIDMap a
      eraseUUID = UM.delete uuid
  in do
    name     %= eraseUUID
    hp       %= eraseUUID
    position %= eraseUUID
    cooldown %= eraseUUID

demandComp :: (HasComponents s, MonadState s m)
           => Component s a
           -> UUID
           -> m a
demandComp comp uuid = do
    result <- getComp uuid comp
    assert (isJust result) $ return (fromJust result)

demandHasComp :: (HasComponents s, MonadState s m)
          => UUID
          -> Component s a
          -> m ()
demandHasComp uuid comp = do
    result <- hasComp uuid comp
    assert result $ return ()

getPrimaryOccupant :: (HasComponents s, MonadState s m)
                   => UUID -> m UUID
getPrimaryOccupant tileUUID = do
    demandHasComp tileUUID tileType
    occs <- uuidSetToList <$> demandComp occupants tileUUID
    types <- forM occs $ \uuid -> demandComp entityType uuid
    let (maxUUID,_) = maximumBy (comparing snd) $ zip occs types
    return maxUUID

