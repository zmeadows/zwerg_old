module Zwerg.Event where

import Zwerg.Class
import Zwerg.Component.All
import Zwerg.Component.UUID (UUID)
import Zwerg.Data.Damage
import qualified Zwerg.Data.Direction as Dir
import Zwerg.Prelude

import Control.Lens
       ((.=), (%=), over, use, to, makeClassy, makeFields, Lens')
import Data.Sequence (Seq, (><), (<|), (|>), ViewL(..), ViewR(..))
import qualified Data.Sequence as S

class EventData a

data IncomingDamageEventData = IncomingDamageEventData
  { _incomingDamageEventDataAttackerUUID :: UUID
  , _incomingDamageEventDataDefenderUUID :: UUID
  , _incomingDamageEventDataDamage :: DamageChain
  } deriving (Show, Eq)

instance EventData IncomingDamageEventData

makeFields ''IncomingDamageEventData

data OutgoingDamageEventData = OutgoingDamageEventData
  { _outgoingDamageEventDataAttackerUUID :: UUID
  , _outgoingDamageEventDataDefenderUUID :: UUID
  , _outgoingDamageEventDataDamageAmount :: Int
  } deriving (Show, Eq)

instance EventData OutgoingDamageEventData

makeFields ''OutgoingDamageEventData

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
  { _weaponAttackAttemptEventDataAttackerUUID :: UUID
  , _weaponAttackAttemptEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackAttemptEventData

instance EventData WeaponAttackAttemptEventData

data WeaponAttackHitEventData = WeaponAttackHitEventData
  { _weaponAttackHitEventDataAttackerUUID :: UUID
  , _weaponAttackHitEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackHitEventData

instance EventData WeaponAttackHitEventData

data WeaponAttackMissEventData = WeaponAttackMissEventData
  { _weaponAttackMissEventDataAttackerUUID :: UUID
  , _weaponAttackMissEventDataDefenderUUID :: UUID
  } deriving (Show, Eq)

makeFields ''WeaponAttackMissEventData

instance EventData WeaponAttackMissEventData

data Event
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
  , direction :: Dir.Direction
  } deriving (Show, Eq)

data TickEventData =
  TickEventData Int
  deriving (Show, Eq)

newtype EventQueue =
  MkEventQueue (Seq Event)
  deriving (Show, Eq)

class HasEventQueue s where
  eventQueue :: Lens' s EventQueue

instance ZWrapped EventQueue (Seq Event) where
  unwrap (MkEventQueue eq) = eq

instance ZEmptiable EventQueue where
  zEmpty = MkEventQueue S.empty
  zIsNull = S.null . unwrap
  zSize = S.length . unwrap

popEvent
  :: (HasEventQueue s, MonadState s m)
  => m (Maybe Event)
popEvent = do
  eq <- use eventQueue
  if | zIsNull eq -> return Nothing
     | otherwise ->
       let (evt :< eq') = S.viewl (unwrap eq)
       in do eventQueue .= MkEventQueue eq'
             return $ Just evt

pushEvent :: Event -> EventQueue -> EventQueue
pushEvent evt (MkEventQueue eq) = MkEventQueue $ eq |> evt

pushEventM
  :: (HasEventQueue s, MonadState s m)
  => Event -> m ()
pushEventM evt = eventQueue %= MkEventQueue . (flip (|>) evt) . unwrap

pushEventsM
  :: (HasEventQueue s, MonadState s m)
  => EventQueue -> m ()
pushEventsM evts = eventQueue %= MkEventQueue . ((><) (unwrap evts)) . unwrap
