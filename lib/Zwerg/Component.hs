module Zwerg.Component where

import Zwerg.Prelude

import Zwerg.Data.Damage
import Zwerg.Data.Equipment
import Zwerg.Data.GridMap
import Zwerg.Data.HP
import Zwerg.Data.Position
import Zwerg.Data.UUIDMap
import Zwerg.Data.UUIDSet (UUIDSet)
import Zwerg.Util

import Data.Text (append)

data Components = Components
  { _name :: NamedUUIDMap Text
  , _description :: NamedUUIDMap Text
  , _species :: NamedUUIDMap Text
  , _glyph :: NamedUUIDMap Glyph
  , _hp :: NamedUUIDMap HP
  , _entityType :: NamedUUIDMap EntityType
  , _position :: NamedUUIDMap Position
  , _cooldown :: NamedUUIDMap Int
  , _equipment :: NamedUUIDMap Equipment
  , _level :: NamedUUIDMap UUID
  , _resistances :: NamedUUIDMap Resistances
  , _tiles :: NamedUUIDMap UUIDSet
  , _ticks :: NamedUUIDMap Int
  , _tileOn :: NamedUUIDMap UUID
  , _tileType :: NamedUUIDMap TileType
  , _tileMap :: NamedUUIDMap (GridMap UUID)
  , _occupants :: NamedUUIDMap UUIDSet
  , _parent :: NamedUUIDMap Parent
  , _children :: NamedUUIDMap UUIDSet
  , _inventory :: NamedUUIDMap UUIDSet
  , _stats :: NamedUUIDMap Stats
  , _blocksPassage :: NamedUUIDMap Bool
  , _blocksVision :: NamedUUIDMap Bool
  , _aiType :: NamedUUIDMap AIType
  , _damageChain :: NamedUUIDMap DamageChain
  , _viewRange :: NamedUUIDMap Double
  , _slot :: NamedUUIDMap EquipmentSlot
  , _itemType :: NamedUUIDMap ItemType
  , _needsRedraw :: NamedUUIDMap Bool
  , _zLevel :: NamedUUIDMap ZLevel
  , _nextUUID :: UUID
  } deriving (Show, Eq, Generic)

makeClassy ''Components

instance Binary Components

type Component a = forall s. HasComponents s =>
                               Lens' s (NamedUUIDMap a)

-- purely for convenience, type synonyms for commonly various monad contexts
type MonadCompState a = forall s m. ( HasCallStack
                                    , HasComponents s
                                    , MonadState s m
                                    , MonadError ZError m
                                    ) =>
                                      m a

type MonadCompStateRand a = forall s m. ( HasCallStack
                                        , HasComponents s
                                        , MonadState s m
                                        , MonadError ZError m
                                        , MonadRandom m
                                        ) =>
                                          m a

type MonadCompRead a = forall s m. ( HasCallStack
                                   , HasComponents s
                                   , MonadReader s m
                                   , MonadError ZError m
                                   ) =>
                                     m a

type MonadCompReadRand a = forall s m. ( HasCallStack
                                       , HasComponents s
                                       , MonadReader s m
                                       , MonadError ZError m
                                       , MonadRandom m
                                       ) =>
                                         m a

-- For running a MonadCompRead function inside the MonadCompState context
readC :: MonadCompRead a -> MonadCompState a
readC x = do
  cs <- use components
  case runReader (runExceptT x) cs of
    Left err -> throwError err
    Right q -> return q

emptyComponents :: Components
emptyComponents =
  Components
  { _name = NamedUUIDMap "name" zEmpty
  , _description = NamedUUIDMap "description" zEmpty
  , _species = NamedUUIDMap "species" zEmpty
  , _glyph = NamedUUIDMap "glyph" zEmpty
  , _hp = NamedUUIDMap "hp" zEmpty
  , _entityType = NamedUUIDMap "entityType" zEmpty
  , _position = NamedUUIDMap "position" zEmpty
  , _cooldown = NamedUUIDMap "cooldown" zEmpty
  , _equipment = NamedUUIDMap "equipment" zEmpty
  , _level = NamedUUIDMap "level" zEmpty
  , _resistances = NamedUUIDMap "resistances" zEmpty
  , _tiles = NamedUUIDMap "tiles" zEmpty
  , _tileOn = NamedUUIDMap "tileOn" zEmpty
  , _ticks = NamedUUIDMap "ticks" zEmpty
  , _tileType = NamedUUIDMap "tileType" zEmpty
  , _tileMap = NamedUUIDMap "tileMap" zEmpty
  , _occupants = NamedUUIDMap "occupants" zEmpty
  , _parent = NamedUUIDMap "parent" zEmpty
  , _children = NamedUUIDMap "children" zEmpty
  , _inventory = NamedUUIDMap "inventory" zEmpty
  , _stats = NamedUUIDMap "stats" zEmpty
  , _blocksPassage = NamedUUIDMap "blocksPassage" zEmpty
  , _blocksVision = NamedUUIDMap "blocksVision" zEmpty
  , _aiType = NamedUUIDMap "aiType" zEmpty
  , _damageChain = NamedUUIDMap "damageChain" zEmpty
  , _viewRange = NamedUUIDMap "viewRange" zEmpty
  , _slot = NamedUUIDMap "slot" zEmpty
  , _itemType = NamedUUIDMap "itemType" zEmpty
  , _needsRedraw = NamedUUIDMap "needsRedraw" zEmpty
  , _zLevel = NamedUUIDMap "zLevel" zEmpty
  , _nextUUID = incUUID playerUUID
  }

popUUID :: MonadCompState UUID
popUUID = do
  newUUID <- use nextUUID
  nextUUID %= incUUID
  return newUUID

{-- STATE --}
{-# INLINEABLE getComp #-}
getComp :: UUID -> Component a -> MonadCompState (Maybe a)
getComp uuid comp = use $ comp . uuidMap . at uuid

{-# INLINEABLE hasComp #-}
hasComp :: UUID -> Component a -> MonadCompState Bool
hasComp uuid comp = use $ comp . uuidMap . to (zContains uuid)

{-# INLINEABLE canViewComp #-}
canViewComp :: UUID -> Component a -> MonadCompRead Bool
canViewComp uuid comp = view $ comp . uuidMap . to (zContains uuid)

{-# INLINEABLE addComp #-}
addComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> a -> m ()
addComp uuid comp dat = (comp . uuidMap) %= zInsert uuid dat

{-# INLINEABLE setComp #-}
setComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> a -> m ()
setComp = addComp

{-# INLINEABLE modComp #-}
modComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> (a -> a) -> m ()
modComp uuid comp f = (comp . uuidMap) %= zAdjust f uuid

{-# INLINEABLE deleteComp #-}
deleteComp
  :: (HasComponents s, MonadState s m)
  => UUID -> Component a -> m ()
deleteComp uuid comp = (comp . uuidMap) %= zRemoveAt uuid

{-# INLINEABLE filterComp #-}
filterComp
  :: (HasComponents s, MonadState s m)
  => Component a -> (a -> Bool) -> m ()
filterComp comp f = (comp . uuidMap) %= zFilter (\(_, x) -> f x)

{-# INLINEABLE demandComp #-}
demandComp :: Component a -> UUID -> MonadCompState a
demandComp comp uuid =
  getComp uuid comp >>= \case
    Just x -> return x
    Nothing -> do
      cn <- use (comp . componentName)
      $(throw) EngineFatal $ append "Missing Component: " cn

{-# INLINEABLE demandHasComp #-}
demandHasComp :: UUID -> Component a -> MonadCompState ()
demandHasComp uuid comp = do
  whenM (not <$> hasComp uuid comp) $ do
    cn <- use (comp . componentName)
    $(throw) EngineFatal $ append "Missing Component: " cn

{-- READER --}
{-# INLINEABLE viewComp #-}
viewComp :: UUID -> Component a -> MonadCompRead (Maybe a)
viewComp uuid comp = view (comp . uuidMap . at uuid)

{-# INLINEABLE demandViewComp #-}
demandViewComp :: Component a -> UUID -> MonadCompRead a
demandViewComp comp uuid =
  viewComp uuid comp >>= \case
    Just x -> return x
    Nothing -> do
      cn <- view (comp . componentName)
      $(throw) EngineFatal $ append "Missing Component: " cn

{-# INLINEABLE demandCanViewComp #-}
demandCanViewComp :: Component a -> UUID -> MonadCompRead ()
demandCanViewComp comp uuid =
  whenM (not <$> canViewComp uuid comp) $ do
    cn <- view (comp . componentName)
    $(throw) EngineFatal $ append "Missing Component: " cn

{-# INLINEABLE (<@>) #-}
(<@>) :: Component a -> UUID -> MonadCompState a
(<@>) = demandComp

{-# INLINEABLE (<~>) #-}
(<~>) :: Component a -> UUID -> MonadCompRead a
(<~>) = demandViewComp

{-# INLINEABLE (<~?>) #-}
(<~?>) :: UUID -> Component a -> MonadCompRead (Maybe a)
(<~?>) = viewComp
