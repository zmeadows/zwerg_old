module Zwerg.Event where
-- TODO: export properly only what is needed

import Zwerg.Component.Position
import Zwerg.Data.Damage
import Zwerg.Prelude
import Zwerg.Random.Distribution

import Data.Sequence (Seq, (><), (|>), ViewL(..))
import qualified Data.Sequence as S

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data IncomingDamageEventData = IncomingDamageEventData
  { _incomingDamageEventDataAttackerUUID :: UUID
  , _incomingDamageEventDataDefenderUUID :: UUID
  , _incomingDamageEventDataDamageAttribute :: DamageAttribute
  , _incomingDamageEventDataDamageDistribution :: Distribution
  } deriving (Show, Eq, Generic)
makeFields ''IncomingDamageEventData
instance Binary IncomingDamageEventData

data OutgoingDamageEventData = OutgoingDamageEventData
  { _outgoingDamageEventDataAttackerUUID :: UUID
  , _outgoingDamageEventDataDefenderUUID :: UUID
  , _outgoingDamageEventDataDamageAmount :: Int
  } deriving (Show, Eq, Generic)
makeFields ''OutgoingDamageEventData
instance Binary OutgoingDamageEventData

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
  { _weaponAttackAttemptEventDataAttackerUUID :: UUID
  , _weaponAttackAttemptEventDataDefenderUUID :: UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackAttemptEventData
instance Binary WeaponAttackAttemptEventData

data WeaponAttackHitEventData = WeaponAttackHitEventData
  { _weaponAttackHitEventDataAttackerUUID :: UUID
  , _weaponAttackHitEventDataDefenderUUID :: UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackHitEventData
instance Binary WeaponAttackHitEventData

data WeaponAttackMissEventData = WeaponAttackMissEventData
  { _weaponAttackMissEventDataAttackerUUID :: UUID
  , _weaponAttackMissEventDataDefenderUUID :: UUID
  } deriving (Show, Eq, Generic)
makeFields ''WeaponAttackMissEventData
instance Binary WeaponAttackMissEventData

data DeathEventData = DeathEventData
  { _deathEventDataDyingUUID :: UUID
  } deriving (Show, Eq, Generic)
makeFields ''DeathEventData
instance Binary DeathEventData

data GenerateEntityEventData = GenerateEntityEventData Int Int
  deriving (Show, Eq, Generic)
instance Binary GenerateEntityEventData

data MoveEntityDirectionEventData = MoveEntityDirectionEventData
  { _moveEntityDirectionEventDataMoverUUID :: UUID
  , _moveEntityDirectionEventDataDirection :: Direction
  } deriving (Show, Eq, Generic)
makeFields ''MoveEntityDirectionEventData
instance Binary MoveEntityDirectionEventData

data MoveEntityEventData = MoveEntityEventData
  { _moveEntityEventDataMoverUUID :: UUID
  , _moveEntityEventDataNewPosition :: Position
  } deriving (Show, Eq, Generic)
makeFields ''MoveEntityEventData
instance Binary MoveEntityEventData

data TickEventData = TickEventData Int
  deriving (Show, Eq, Generic)
instance Binary TickEventData

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
  deriving (Show, Eq, Generic)

instance Binary ZwergEvent

newtype ZwergEventQueue = MkZwergEventQueue (Seq ZwergEvent)
  deriving (Show, Eq, Generic)

instance Binary ZwergEventQueue

class HasZwergEventQueue s where
  eventQueue :: Lens' s ZwergEventQueue

instance ZWrapped ZwergEventQueue (Seq ZwergEvent) where
  unwrap (MkZwergEventQueue eq) = eq

instance ZEmptiable ZwergEventQueue where
  zEmpty  = MkZwergEventQueue S.empty
  zIsNull = S.null . unwrap
  zSize   = S.length . unwrap

popEvent :: (HasZwergEventQueue s, MonadState s m) => m (Maybe ZwergEvent)
popEvent = do
  eq <- use eventQueue
  if | zIsNull eq -> return Nothing
     | otherwise ->
       let (evt :< eq') = S.viewl (unwrap eq)
       in do eventQueue .= MkZwergEventQueue eq'
             return $ Just evt

pushEvent :: ZwergEvent -> ZwergEventQueue -> ZwergEventQueue
pushEvent evt (MkZwergEventQueue eq) = MkZwergEventQueue $ eq |> evt

pushEventM :: (HasZwergEventQueue s, MonadState s m) => ZwergEvent -> m ()
pushEventM evt = eventQueue %= MkZwergEventQueue . (|> evt) . unwrap

-- | This template haskell is purely for avoiding boiler plate code.
newEvent :: Language.Haskell.TH.Syntax.Quasi m => Text -> m Exp
newEvent "MoveEntity"     = runQ [| \a b -> pushEventM $ MoveEntityEvent $ MoveEntityEventData a b|]
newEvent "IncomingDamage" = runQ [| \a b c d -> pushEventM $ IncomingDamageEvent $ IncomingDamageEventData a b c d|]
newEvent "OutgoingDamage" = runQ [| \a b c -> pushEventM $ OutgoingDamageEvent $ OutgoingDamageEventData a b c|]
newEvent _                = runQ [|"INVALID EVENT TYPE PASSED TO TEMPLATE HASKELL FUNCTION 'newEvent'"|]

mergeEventsM :: (HasZwergEventQueue s, MonadState s m)
             => ZwergEventQueue -> m ()
mergeEventsM evts = eventQueue %= MkZwergEventQueue . (><) (unwrap evts) . unwrap
