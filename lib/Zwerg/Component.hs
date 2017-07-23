module Zwerg.Component where

import Zwerg.Component.All
import Zwerg.Data.Damage
import Zwerg.Data.Equipment
import Zwerg.Data.UUIDMap
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Prelude
import Zwerg.Util

import Control.Exception.Base (assert)
import Data.Text (append)

data Components = Components
  { _name           :: NamedUUIDMap Text
  , _glyph          :: NamedUUIDMap Glyph
  , _hp             :: NamedUUIDMap HP
  , _entityType     :: NamedUUIDMap EntityType
  , _position       :: NamedUUIDMap Position
  , _cooldown       :: NamedUUIDMap Int
  , _equipment      :: NamedUUIDMap Equipment
  , _level          :: NamedUUIDMap UUID
  , _tiles          :: NamedUUIDMap UUIDSet
  , _ticks          :: NamedUUIDMap Int
  , _tileOn         :: NamedUUIDMap UUID
  , _tileType       :: NamedUUIDMap TileType
  , _tileMap        :: NamedUUIDMap TileMap
  , _occupants      :: NamedUUIDMap UUIDSet
  , _parent         :: NamedUUIDMap Parent
  , _children       :: NamedUUIDMap UUIDSet
  , _stats          :: NamedUUIDMap Stats
  , _blocksPassage  :: NamedUUIDMap Bool
  , _blocksVision   :: NamedUUIDMap Bool
  , _aiType         :: NamedUUIDMap AIType
  , _damageChain    :: NamedUUIDMap DamageChain
  , _viewRange      :: NamedUUIDMap Double
  , _equippableSlot :: NamedUUIDMap EquippableSlot
  , _itemType       :: NamedUUIDMap ItemType
  , _needsRedraw    :: NamedUUIDMap Bool
  , _nextUUID       :: UUID
  } deriving (Show, Eq, Generic)
makeClassy ''Components

instance Binary Components

type Component a = forall s. HasComponents s => Lens' s (NamedUUIDMap a)

-- Basic context for manipulating entity components
type MonadCompState a = forall s m. ( HasComponents s
                                    , MonadError ZError m
                                    , MonadState s m
                                    ) => m a

-- Basic context for reading entity component values
type MonadCompReader a = forall s m. ( HasComponents s
                                     , MonadError ZError m
                                     , MonadReader s m
                                     ) => m a

-- For running a MonadCompReader function inside the MonadCompState context
readC :: MonadCompReader a -> MonadCompState a
readC x = do
  cs <- use components
  case runReader (runExceptT x) cs of
    Left err -> throwError err
    Right q -> return q

emptyComponents :: Components
emptyComponents =
  Components
  { _name           = NamedUUIDMap "name"           zEmpty
  , _glyph          = NamedUUIDMap "glyph"          zEmpty
  , _hp             = NamedUUIDMap "hp"             zEmpty
  , _entityType     = NamedUUIDMap "entityType"     zEmpty
  , _position       = NamedUUIDMap "position"       zEmpty
  , _cooldown       = NamedUUIDMap "cooldown"       zEmpty
  , _equipment      = NamedUUIDMap "equipment"      zEmpty
  , _level          = NamedUUIDMap "level"          zEmpty
  , _tiles          = NamedUUIDMap "tiles"          zEmpty
  , _tileOn         = NamedUUIDMap "tileOn"         zEmpty
  , _ticks          = NamedUUIDMap "ticks"          zEmpty
  , _tileType       = NamedUUIDMap "tileType"       zEmpty
  , _tileMap        = NamedUUIDMap "tileMap"        zEmpty
  , _occupants      = NamedUUIDMap "occupants"      zEmpty
  , _parent         = NamedUUIDMap "parent"         zEmpty
  , _children       = NamedUUIDMap "children"       zEmpty
  , _stats          = NamedUUIDMap "stats"          zEmpty
  , _blocksPassage  = NamedUUIDMap "blocksPassage"  zEmpty
  , _blocksVision   = NamedUUIDMap "blocksVision"   zEmpty
  , _aiType         = NamedUUIDMap "aiType"         zEmpty
  , _damageChain    = NamedUUIDMap "damageChain"    zEmpty
  , _viewRange      = NamedUUIDMap "viewRange"      zEmpty
  , _equippableSlot = NamedUUIDMap "equippableSlot" zEmpty
  , _itemType       = NamedUUIDMap "itemType"       zEmpty
  , _needsRedraw    = NamedUUIDMap "needsRedraw"    zEmpty
  , _nextUUID       = playerUUID + 1
  }

popUUID :: MonadCompState UUID
popUUID = do
  newUUID <- use nextUUID
  nextUUID %= (+ 1)
  return newUUID

{-- STATE --}
getComp :: UUID -> Component a -> MonadCompState (Maybe a)
getComp uuid comp = use $ comp . uuidMap . at uuid

hasComp :: UUID -> Component a -> MonadCompState Bool
hasComp uuid comp = use $ comp . uuidMap . to (zContains uuid)

addComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> a -> m ()
addComp uuid comp dat = (comp . uuidMap) %= zInsert uuid dat

setComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> a -> m ()
setComp = addComp

modComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> (a -> a) -> m ()
modComp uuid comp f = (comp . uuidMap) %= zAdjust f uuid

deleteComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> m ()
deleteComp uuid comp = (comp . uuidMap) %= zRemoveAt uuid

filterComp
  :: (HasComponents s, MonadState s m)
  => Component a -> (a -> Bool) -> m ()
filterComp comp f = (comp . uuidMap) %= zFilter (\(_, x) -> f x)

-- TODO: define binary operators for demandComp, demandViewComp, and more(?)
demandComp :: Component a -> UUID -> MonadCompState a
demandComp comp uuid = do
  result <- getComp uuid comp
  cn <- use (comp . componentName)
  whenJustErr
    result
    (ZError __FILE__ __LINE__ EngineFatal $ append "Missing Component: " cn)
    return

-- (<.>) :: Component a -> UUID -> MonadCompState a
-- (<.>) = demandComp

demandHasComp :: UUID -> Component a -> MonadCompState ()
demandHasComp uuid comp = do
  result <- hasComp uuid comp
  assert result $ return ()

{-- READER --}
viewComp :: UUID -> Component a -> MonadCompReader (Maybe a)
viewComp uuid comp = view (comp . uuidMap . at uuid)

demandViewComp :: Component a -> UUID -> MonadCompReader a
demandViewComp comp uuid = do
  result <- view (comp . uuidMap . at uuid)
  cn <- view (comp . componentName)
  whenJustErr
    result
    (ZError __FILE__ __LINE__ EngineFatal $ append "Missing Component: " cn)
    return

(<~>) :: Component a -> UUID -> MonadCompReader a
(<~>) = demandViewComp

(<@>) :: Component a -> UUID -> MonadCompState a
(<@>) = demandComp

