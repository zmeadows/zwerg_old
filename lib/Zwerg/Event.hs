module Zwerg.Event where

import Zwerg.Data.Damage
import Zwerg.Prelude

import Data.Sequence (Seq, (><), (|>), ViewL(..))
import qualified Data.Sequence as S

class ZwergEventData a

data IncomingDamageEventData = IncomingDamageEventData
  { _incomingDamageEventDataAttackerUUID :: UUID
  , _incomingDamageEventDataDefenderUUID :: UUID
  , _incomingDamageEventDataDamage :: DamageChain
  } deriving (Show, Eq)

instance ZwergEventData IncomingDamageEventData

makeFields ''IncomingDamageEventData

data OutgoingDamageEventData = OutgoingDamageEventData
  { _outgoingDamageEventDataAttackerUUID :: UUID
  , _outgoingDamageEventDataDefenderUUID :: UUID
  , _outgoingDamageEventDataDamageAmount :: Int
  } deriving (Show, Eq)

instance ZwergEventData OutgoingDamageEventData

makeFields ''OutgoingDamageEventData

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
  { _weaponAttackAttemptEventDataAttackerUUID :: UUID
  , _weaponAttackAttemptEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackAttemptEventData

instance ZwergEventData WeaponAttackAttemptEventData

data WeaponAttackHitEventData = WeaponAttackHitEventData
  { _weaponAttackHitEventDataAttackerUUID :: UUID
  , _weaponAttackHitEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackHitEventData

instance ZwergEventData WeaponAttackHitEventData

data WeaponAttackMissEventData = WeaponAttackMissEventData
  { _weaponAttackMissEventDataAttackerUUID :: UUID
  , _weaponAttackMissEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackMissEventData

instance ZwergEventData WeaponAttackMissEventData

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

data DeathEventData =
  DeathEventData Int
  deriving (Show, Eq)

data GenerateEntityEventData =
  GenerateEntityEventData Int
  deriving (Show, Eq)

data MoveEntityEventData =
  MoveEntityEventData Int
  deriving (Show, Eq)

data MoveEntityDirectionEventData = MoveEntityDirectionEventData
  { moverUUID :: UUID
  , direction :: Direction
  } deriving (Show, Eq)

data TickEventData =
  TickEventData Int
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
