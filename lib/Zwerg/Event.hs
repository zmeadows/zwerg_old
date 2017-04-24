module Zwerg.Event where

import Zwerg.Component.Position
import Zwerg.Data.Damage
import Zwerg.Prelude

import Data.Sequence (Seq, (><), (|>), ViewL(..))
import qualified Data.Sequence as S

data IncomingDamageEventData = IncomingDamageEventData
  { _incomingDamageEventDataAttackerUUID :: UUID
  , _incomingDamageEventDataDefenderUUID :: UUID
  , _incomingDamageEventDataDamage :: DamageChain
  } deriving (Show, Eq)

makeFields ''IncomingDamageEventData

data OutgoingDamageEventData = OutgoingDamageEventData
  { _outgoingDamageEventDataAttackerUUID :: UUID
  , _outgoingDamageEventDataDefenderUUID :: UUID
  , _outgoingDamageEventDataDamageAmount :: Int
  } deriving (Show, Eq)

makeFields ''OutgoingDamageEventData

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
  { _weaponAttackAttemptEventDataAttackerUUID :: UUID
  , _weaponAttackAttemptEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackAttemptEventData

data WeaponAttackHitEventData = WeaponAttackHitEventData
  { _weaponAttackHitEventDataAttackerUUID :: UUID
  , _weaponAttackHitEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackHitEventData

data WeaponAttackMissEventData = WeaponAttackMissEventData
  { _weaponAttackMissEventDataAttackerUUID :: UUID
  , _weaponAttackMissEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackMissEventData

data DeathEventData = DeathEventData
  { _deathEventDataDyingUUID :: UUID
  } deriving (Show, Eq)

makeFields ''DeathEventData

data GenerateEntityEventData =
  GenerateEntityEventData Int
  deriving (Show, Eq)

data MoveEntityDirectionEventData = MoveEntityDirectionEventData
  { _moveEntityDirectionEventDataMoverUUID :: UUID
  , _moveEntityDirectionEventDataDirection :: Direction
  } deriving (Show, Eq)

makeFields ''MoveEntityDirectionEventData

data MoveEntityEventData = MoveEntityEventData
  { _moveEntityEventDataMoverUUID :: UUID
  , _moveEntityEventDataNewPosition :: Position
  } deriving (Show, Eq)

makeFields ''MoveEntityEventData

data TickEventData =
  TickEventData Int
  deriving (Show, Eq)

data ZwergEvent
  = IncomingDamageEvent IncomingDamageEventData
  | OutgoingDamageEvent OutgoingDamageEventData
  | WeaponAttackAttemptEvent WeaponAttackAttemptEventData
  | WeaponAttackHitEvent WeaponAttackHitEventData
  | WeaponAttackMissEvent WeaponAttackMissEventData
  | DeathEvent DeathEventData
  | GenerateEntityEvent GenerateEntityEventData
  | MoveEntityEvent MoveEntityEventData
  | MoveEntityDirectionEvent MoveEntityDirectionEventData
  | TickEvent TickEventData
  deriving (Show, Eq)

newtype ZwergEventQueue =
  MkZwergEventQueue (Seq ZwergEvent)
  deriving (Show, Eq)

class HasZwergEventQueue s where
  eventQueue :: Lens' s ZwergEventQueue

instance ZWrapped ZwergEventQueue (Seq ZwergEvent) where
  unwrap (MkZwergEventQueue eq) = eq

instance ZEmptiable ZwergEventQueue where
  zEmpty = MkZwergEventQueue S.empty
  zIsNull = S.null . unwrap
  zSize = S.length . unwrap

popEvent
  :: (HasZwergEventQueue s, MonadState s m)
  => m (Maybe ZwergEvent)
popEvent = do
  eq <- use eventQueue
  if | zIsNull eq -> return Nothing
     | otherwise ->
       let (evt :< eq') = S.viewl (unwrap eq)
       in do eventQueue .= MkZwergEventQueue eq'
             return $ Just evt

pushEvent :: ZwergEvent -> ZwergEventQueue -> ZwergEventQueue
pushEvent evt (MkZwergEventQueue eq) = MkZwergEventQueue $ eq |> evt

pushEventM
  :: (HasZwergEventQueue s, MonadState s m)
  => ZwergEvent -> m ()
pushEventM evt = eventQueue %= MkZwergEventQueue . (flip (|>) evt) . unwrap

mergeEventsM
  :: (HasZwergEventQueue s, MonadState s m)
  => ZwergEventQueue -> m ()
mergeEventsM evts =
  eventQueue %= MkZwergEventQueue . ((><) (unwrap evts)) . unwrap
