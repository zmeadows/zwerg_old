module Zwerg.Component where

import Zwerg.Class
import Zwerg.Component.All
import Zwerg.Data.Damage
import Zwerg.Data.Error
import Zwerg.Data.UUIDMap
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude
import Zwerg.Util

import Control.Exception.Base (assert)
import Data.Binary
import Data.Text (Text, append)
import GHC.Generics (Generic)

import Control.Lens (makeClassy, Lens', at, use, to, (%=), view)

data Components = Components
  { _name :: NamedUUIDMap Text
  , _glyph :: NamedUUIDMap Glyph
  , _hp :: NamedUUIDMap HP
  , _entityType :: NamedUUIDMap EntityType
  , _position :: NamedUUIDMap Position
  , _cooldown :: NamedUUIDMap Int
  , _equipment :: NamedUUIDMap Equipment
  , _baseDamage :: NamedUUIDMap Int
  , _level :: NamedUUIDMap UUID
  , _tileMap :: NamedUUIDMap TileMap
  , _tiles :: NamedUUIDMap UUIDSet
  , _ticks :: NamedUUIDMap Int
  , _tileType :: NamedUUIDMap TileType
  , _occupants :: NamedUUIDMap UUIDSet
  , _parent :: NamedUUIDMap Parent
  , _children :: NamedUUIDMap UUIDSet
  , _stats :: NamedUUIDMap Stats
  , _blocked :: NamedUUIDMap Bool
  , _needsRedraw :: NamedUUIDMap Bool
  , _aiType :: NamedUUIDMap AIType
  , _damageChain :: NamedUUIDMap DamageChain
  , _viewRange :: NamedUUIDMap Double
  } deriving (Show, Eq, Generic)

makeClassy ''Components

instance Binary Components

type Component s a = HasComponents s =>
                       Lens' s (NamedUUIDMap a)

emptyComponents :: Components
emptyComponents =
  Components
  { _name = NamedUUIDMap "name" zEmpty
  , _glyph = NamedUUIDMap "glyph" zEmpty
  , _hp = NamedUUIDMap "hp" zEmpty
  , _entityType = NamedUUIDMap "entityType" zEmpty
  , _position = NamedUUIDMap "position" zEmpty
  , _cooldown = NamedUUIDMap "cooldown" zEmpty
  , _equipment = NamedUUIDMap "equipment" zEmpty
  , _baseDamage = NamedUUIDMap "baseDamage" zEmpty
  , _level = NamedUUIDMap "level" zEmpty
  , _tileMap = NamedUUIDMap "tileMap" zEmpty
  , _tiles = NamedUUIDMap "tiles" zEmpty
  , _ticks = NamedUUIDMap "ticks" zEmpty
  , _tileType = NamedUUIDMap "tileType" zEmpty
  , _occupants = NamedUUIDMap "occupants" zEmpty
  , _parent = NamedUUIDMap "parent" zEmpty
  , _children = NamedUUIDMap "children" zEmpty
  , _stats = NamedUUIDMap "stats" zEmpty
  , _blocked = NamedUUIDMap "blocked" zEmpty
  , _needsRedraw = NamedUUIDMap "needsRedraw" zEmpty
  , _aiType = NamedUUIDMap "aiType" zEmpty
  , _damageChain = NamedUUIDMap "damageChain" zEmpty
  , _viewRange = NamedUUIDMap "viewRange" zEmpty
  }

{-- STATE --}
getComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> m (Maybe a)
getComp uuid comp = use $ comp . uuidMap . at uuid

hasComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> m Bool
hasComp uuid comp = use $ comp . uuidMap . to (zContains uuid)

addComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> a -> m ()
addComp uuid comp dat = (comp . uuidMap) %= zInsert uuid dat

setComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> a -> m ()
setComp = addComp

modComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> (a -> a) -> m ()
modComp uuid comp f = (comp . uuidMap) %= zAdjust f uuid

deleteComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> m ()
deleteComp uuid comp = (comp . uuidMap) %= (zRemoveAt uuid)

filterComp
  :: (HasComponents s, MonadState s m)
  => Component s a -> (a -> Bool) -> m ()
filterComp comp f = (comp . uuidMap) %= zFilter (\(_, x) -> f x)

demandComp
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => Component s a -> UUID -> m a
demandComp comp uuid = do
  result <- getComp uuid comp
  cn <- use (comp . componentName)
  whenJustErr
    result
    (ZError __FILE__ __LINE__ Fatal $ append "Missing Component" cn)
    return

demandHasComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component s a -> m ()
demandHasComp uuid comp = do
  result <- hasComp uuid comp
  assert result $ return ()

{-- READER --}
viewComp
  :: (HasComponents r, MonadReader r m)
  => UUID -> Component r a -> m (Maybe a)
viewComp uuid comp = view (comp . uuidMap . at uuid)

demandViewComp
  :: (HasComponents r, MonadError ZError m, MonadReader r m)
  => UUID -> Component r a -> m a
demandViewComp uuid comp = do
  result <- view (comp . uuidMap . at uuid)
  cn <- view (comp . componentName)
  whenJustErr
    result
    (ZError __FILE__ __LINE__ Fatal $ append "Missing Component: " cn)
    return
