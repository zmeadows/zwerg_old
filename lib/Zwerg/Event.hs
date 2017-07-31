module Zwerg.Event where

import Zwerg.Prelude
import Zwerg.Data.Damage
import Zwerg.Data.Position
import Zwerg.Random.Distribution

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data IncomingDamageEventData = IncomingDamageEventData
  { _incomingDamageEventDataAttackerUUID :: !UUID
  , _incomingDamageEventDataDefenderUUID :: !UUID
  , _incomingDamageEventDataDamageAttribute :: !DamageAttribute
  , _incomingDamageEventDataDamageDistribution :: !Distribution
  } deriving (Show, Eq, Generic)
makeFields ''IncomingDamageEventData
instance Binary IncomingDamageEventData

data OutgoingDamageEventData = OutgoingDamageEventData
  { _outgoingDamageEventDataAttackerUUID :: !UUID
  , _outgoingDamageEventDataDefenderUUID :: !UUID
  , _outgoingDamageEventDataDamageAmount :: !Int
  } deriving (Show, Eq, Generic)
makeFields ''OutgoingDamageEventData
instance Binary OutgoingDamageEventData

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
  { _weaponAttackAttemptEventDataAttackerUUID :: !UUID
  , _weaponAttackAttemptEventDataDefenderUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackAttemptEventData
instance Binary WeaponAttackAttemptEventData

data WeaponAttackHitEventData = WeaponAttackHitEventData
  { _weaponAttackHitEventDataAttackerUUID :: !UUID
  , _weaponAttackHitEventDataDefenderUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackHitEventData
instance Binary WeaponAttackHitEventData

data WeaponAttackMissEventData = WeaponAttackMissEventData
  { _weaponAttackMissEventDataAttackerUUID :: !UUID
  , _weaponAttackMissEventDataDefenderUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackMissEventData
instance Binary WeaponAttackMissEventData

data DeathEventData = DeathEventData
  { _deathEventDataDyingUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''DeathEventData
instance Binary DeathEventData

data GenerateEntityEventData = GenerateEntityEventData !Int !Int
  deriving (Show, Eq, Generic)
instance Binary GenerateEntityEventData

data MoveEntityDirectionEventData = MoveEntityDirectionEventData
  { _moveEntityDirectionEventDataMoverUUID :: !UUID
  , _moveEntityDirectionEventDataDirection :: !Direction
  } deriving (Show, Eq, Generic)
makeFields ''MoveEntityDirectionEventData
instance Binary MoveEntityDirectionEventData

data MoveEntityEventData = MoveEntityEventData
  { _moveEntityEventDataMoverUUID :: !UUID
  , _moveEntityEventDataNewPosition :: !Position
  } deriving (Show, Eq, Generic)
makeFields ''MoveEntityEventData
instance Binary MoveEntityEventData

data EntityLeftTileEventData = EntityLeftTileEventData
  { _entityLeftTileEventDataLeaverUUID :: !UUID
  , _entityLeftTileEventDataLeftTileUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''EntityLeftTileEventData
instance Binary EntityLeftTileEventData

data EntityReachedTileEventData = EntityReachedTileEventData
  { _entityLeftTileEventDataReacherUUID :: !UUID
  , _entityLeftTileEventDataReachedTileUUID :: !UUID
  } deriving (Show, Eq, Generic)
makeFields ''EntityReachedTileEventData
instance Binary EntityReachedTileEventData

data TickEventData = TickEventData !Int
  deriving (Show, Eq, Generic)
instance Binary TickEventData

data ZwergEvent
  = IncomingDamageEvent !IncomingDamageEventData
  | OutgoingDamageEvent !OutgoingDamageEventData
  | WeaponAttackAttemptEvent !WeaponAttackAttemptEventData
  | WeaponAttackHitEvent !WeaponAttackHitEventData
  | WeaponAttackMissEvent !WeaponAttackMissEventData
  | DeathEvent !DeathEventData
  | GenerateEntityEvent !GenerateEntityEventData
  | MoveEntityEvent !MoveEntityEventData
  | MoveEntityDirectionEvent !MoveEntityDirectionEventData
  | TickEvent !TickEventData
  | EntityLeftTileEvent EntityLeftTileEventData
  | EntityReachedTileEvent EntityReachedTileEventData
  deriving (Show, Eq, Generic)
instance Binary ZwergEvent

-- | This template haskell is purely for avoiding boiler plate code.
newEvent :: Language.Haskell.TH.Syntax.Quasi m => Text -> m Exp
newEvent "IncomingDamage"      = runQ [| \a b c d -> pushEventM $ IncomingDamageEvent $ IncomingDamageEventData a b c d|]
newEvent "OutgoingDamage"      = runQ [| \a b c -> pushEventM $ OutgoingDamageEvent $ OutgoingDamageEventData a b c|]
newEvent "WeaponAttackAttempt" = runQ [| \a b -> pushEventM $ WeaponAttackAttemptEvent $ WeaponAttackAttemptEventData a b|]
newEvent "WeaponAttackHit"     = runQ [| \a b -> pushEventM $ WeaponAttackHitEvent $ WeaponAttackHitEventData a b|]
newEvent "WeaponAttackMiss"    = runQ [| \a b -> pushEventM $ WeaponAttackMissEvent $ WeaponAttackMissEventData a b|]
newEvent "MoveEntity"          = runQ [| \a b -> pushEventM $ MoveEntityEvent $ MoveEntityEventData a b|]
newEvent "MoveEntityDirection" = runQ [| \a b -> pushEventM $ MoveEntityDirectionEvent $ MoveEntityDirectionEventData a b|]
newEvent "EntityLeftTile"      = runQ [| \a b -> pushEventM $ EntityLeftTileEvent $ EntityLeftTileEventData a b|]
newEvent "EntityReachedTile"      = runQ [| \a b -> pushEventM $ EntityReachedTileEvent $ EntityReachedTileEventData a b|]
newEvent _                     = runQ [|"INVALID EVENT TYPE PASSED TO TEMPLATE HASKELL FUNCTION 'newEvent'"|]
