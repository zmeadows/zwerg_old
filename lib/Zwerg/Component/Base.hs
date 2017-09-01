module Zwerg.Component.Base
    ( Components
    , Component
    , HasComponents(..)
    , MonadCompRead
    , MonadCompReadRand
    , MonadCompState
    , MonadCompStateRand
    , addComp
    , canViewComp
    , deleteComp
    , demandComp
    , demandViewComp
    , filterComp
    , getComp
    , getCompName
    , getCompUUIDMap
    , getComponents
    , hasComp
    , modComp
    , popUUID
    , rc
    , rcr
    , setComp
    , viewComp
    , viewCompName
    , viewCompUUIDMap
    , viewComponents
    ) where

import Zwerg.Prelude

import Zwerg.Data.Damage
import Zwerg.Data.Equipment
import Zwerg.Data.Glyph
import Zwerg.Data.GridMap
import Zwerg.Data.HP
import Zwerg.Data.Position
import Zwerg.Data.UUIDMap
import Zwerg.Data.UUIDSet
import Zwerg.Debug
import Zwerg.Random.RanGen
import Zwerg.UI.GlyphMap

import Control.Monad.Random (evalRandT)
import Lens.Micro.Platform (makeClassy, Lens', (%=), use, view, _2, _1)

data Components = Components
    { _name          :: (Text, UUIDMap Text          )
    , _description   :: (Text, UUIDMap Text          )
    , _species       :: (Text, UUIDMap Text          )
    , _glyph         :: (Text, UUIDMap Glyph         )
    , _hp            :: (Text, UUIDMap HP            )
    , _entityType    :: (Text, UUIDMap EntityType    )
    , _position      :: (Text, UUIDMap Position      )
    , _cooldown      :: (Text, UUIDMap Int           )
    , _equipment     :: (Text, UUIDMap Equipment     )
    , _level         :: (Text, UUIDMap UUID          )
    , _resistances   :: (Text, UUIDMap Resistances   )
    , _tiles         :: (Text, UUIDMap UUIDSet       )
    , _ticks         :: (Text, UUIDMap Int           )
    , _tileOn        :: (Text, UUIDMap UUID          )
    , _connectedTo   :: (Text, UUIDMap UUID          )
    , _tileType      :: (Text, UUIDMap TileType      )
    , _tileMap       :: (Text, UUIDMap (GridMap UUID))
    , _glyphMap      :: (Text, UUIDMap GlyphMap      )
    , _occupants     :: (Text, UUIDMap UUIDSet       )
    , _parent        :: (Text, UUIDMap Parent        )
    , _children      :: (Text, UUIDMap UUIDSet       )
    , _inventory     :: (Text, UUIDMap UUIDSet       )
    , _stats         :: (Text, UUIDMap Stats         )
    , _blocksPassage :: (Text, UUIDMap Bool          )
    , _blocksVision  :: (Text, UUIDMap Bool          )
    , _aiType        :: (Text, UUIDMap AIType        )
    , _damageChain   :: (Text, UUIDMap DamageChain   )
    , _viewRange     :: (Text, UUIDMap Int           )
    , _slot          :: (Text, UUIDMap EquipmentSlot )
    , _itemType      :: (Text, UUIDMap ItemType      )
    , _zLevel        :: (Text, UUIDMap ZLevel        )
    , _nextUUID      :: UUID }
  deriving stock Generic
  deriving anyclass Binary
makeClassy ''Components

type Component a = forall s. HasComponents s => Lens' s (Text, UUIDMap a)

type MonadCompState a = forall s m. ( HasCallStack
                                    , HasComponents s
                                    , MonadState s m
                                    ) => m a

type MonadCompStateRand a = forall s m. ( HasCallStack
                                        , HasComponents s
                                        , MonadState s m
                                        , MonadRandom m
                                        ) => m a

type MonadCompRead a = forall s m. ( HasCallStack
                                   , HasComponents s
                                   , MonadReader s m
                                   ) => m a

type MonadCompReadRand a = forall s m. ( HasCallStack
                                       , HasComponents s
                                       , MonadReader s m
                                       , MonadRandom m
                                       ) => m a

-- For running a MonadCompRead function inside the MonadCompState context
{-# INLINABLE rc #-}
rc :: MonadCompRead a -> MonadCompState a
rc x = (runReader x) <$> use components

-- For running a MonadCompRead function inside the MonadCompState context
{-# INLINABLE rcr #-}
rcr :: MonadCompReadRand a -> MonadCompStateRand a
rcr x = do
  ranWord <- getRandom
  runReader (evalRandT x $ pureRanGen ranWord) <$> use components

emptyComponents :: Components
emptyComponents = Components
    { _name          = ("name"          , zDefault)
    , _description   = ("description"   , zDefault)
    , _species       = ("species"       , zDefault)
    , _glyph         = ("glyph"         , zDefault)
    , _hp            = ("hp"            , zDefault)
    , _entityType    = ("entityType"    , zDefault)
    , _position      = ("position"      , zDefault)
    , _cooldown      = ("cooldown"      , zDefault)
    , _equipment     = ("equipment"     , zDefault)
    , _level         = ("level"         , zDefault)
    , _resistances   = ("resistances"   , zDefault)
    , _tiles         = ("tiles"         , zDefault)
    , _tileOn        = ("tileOn"        , zDefault)
    , _connectedTo   = ("connectedTo"   , zDefault)
    , _ticks         = ("ticks"         , zDefault)
    , _tileType      = ("tileType"      , zDefault)
    , _tileMap       = ("tileMap"       , zDefault)
    , _glyphMap      = ("glyphMap"      , zDefault)
    , _occupants     = ("occupants"     , zDefault)
    , _parent        = ("parent"        , zDefault)
    , _children      = ("children"      , zDefault)
    , _inventory     = ("inventory"     , zDefault)
    , _stats         = ("stats"         , zDefault)
    , _blocksPassage = ("blocksPassage" , zDefault)
    , _blocksVision  = ("blocksVision"  , zDefault)
    , _aiType        = ("aiType"        , zDefault)
    , _damageChain   = ("damageChain"   , zDefault)
    , _viewRange     = ("viewRange"     , zDefault)
    , _slot          = ("slot"          , zDefault)
    , _itemType      = ("itemType"      , zDefault)
    , _zLevel        = ("zLevel"        , zDefault)
    , _nextUUID      = incUUID playerUUID
    }

instance ZDefault Components where
    zDefault = emptyComponents

{-# INLINABLE popUUID #-}
popUUID :: MonadCompState UUID
popUUID = do
    newUUID <- use nextUUID
    nextUUID %= incUUID
    return newUUID

{-# INLINABLE getComp #-}
getComp :: UUID -> Component a -> MonadCompState (Maybe a)
getComp uuid comp = zLookup uuid . snd <$> use comp

{-# INLINABLE viewComp #-}
viewComp :: UUID -> Component a -> MonadCompRead (Maybe a)
viewComp uuid comp = zLookup uuid . snd <$> view comp

{-# INLINABLE hasComp #-}
hasComp :: UUID -> Component a -> MonadCompState Bool
hasComp uuid comp = zContains uuid . snd <$> use comp

{-# INLINABLE canViewComp #-}
canViewComp :: UUID -> Component a -> MonadCompRead Bool
canViewComp uuid comp = zContains uuid . snd <$> view comp

{-# INLINABLE addComp #-}
addComp :: (HasComponents s, MonadState s m) => UUID -> Component a -> a -> m ()
addComp uuid comp dat = (comp . _2) %= zInsert uuid dat

{-# INLINABLE setComp #-}
setComp :: (HasComponents s, MonadState s m) => UUID -> Component a -> a -> m ()
setComp = addComp

{-# INLINABLE modComp #-}
modComp :: (HasComponents s, MonadState s m) => UUID -> Component a -> (a -> a) -> m ()
modComp uuid comp f = (comp . _2) %= zModifyAt f uuid

{-# INLINABLE deleteComp #-}
deleteComp :: (HasComponents s, MonadState s m) => UUID -> Component a -> m ()
deleteComp uuid comp = (comp . _2) %= zRemoveAt uuid

{-# INLINABLE filterComp #-}
filterComp :: (HasComponents s, MonadState s m) => Component a -> (a -> Bool) -> m ()
filterComp comp f = (comp . _2) %= zFilter (\(_, x) -> f x)

{-# INLINABLE demandComp #-}
demandComp :: ZDefault a => Component a -> UUID -> MonadCompState a
demandComp comp uuid =
  getComp uuid comp >>= \case
    Just x -> return x
    Nothing -> do
      cn <- use (comp . _1)
      debug $ "Missing Component: " <> cn
      return zDefault

{-# INLINABLE demandViewComp #-}
demandViewComp :: ZDefault a => Component a -> UUID -> MonadCompRead a
demandViewComp comp uuid =
  viewComp uuid comp >>= \case
    Just x -> return x
    Nothing -> do
      cn <- view (comp . _1)
      debug $ "Missing Component: " <> cn
      return zDefault

{-# INLINABLE getCompName #-}
getCompName :: (HasComponents s, MonadState s m) => Component a -> m Text
getCompName comp = fst <$> use comp

{-# INLINABLE viewCompName #-}
viewCompName :: (HasComponents s, MonadReader s m) => Component a -> m Text
viewCompName comp = fst <$> view comp

{-# INLINABLE getCompUUIDMap #-}
getCompUUIDMap :: (HasComponents s, MonadState s m) => Component a -> m (UUIDMap a)
getCompUUIDMap comp = snd <$> use comp

{-# INLINABLE viewCompUUIDMap #-}
viewCompUUIDMap :: (HasComponents s, MonadReader s m) => Component a -> m (UUIDMap a)
viewCompUUIDMap comp = snd <$> view comp

{-# INLINABLE getComponents #-}
getComponents :: (HasComponents s, MonadState s m) => m Components
getComponents = use components

{-# INLINABLE viewComponents #-}
viewComponents :: (HasComponents s, MonadReader s m) => m Components
viewComponents = view components
