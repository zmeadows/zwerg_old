module Zwerg.Event where

import Zwerg.Class
import Zwerg.Component.All
import Zwerg.Component.UUID (UUID)
import qualified Zwerg.Data.Direction as Dir
import Zwerg.Prelude

import Control.Lens ((.=), (%=), over, use, to, makeClassy, Lens')
import Data.Sequence (Seq, (<|), (|>), ViewL(..), ViewR(..))
import qualified Data.Sequence as S

class EventData a

data Event
  = IncomingDamageEvent IncomingDamageEventData
  | OutgoingDamageEvent OutgoingDamageEventData
  | PhysicalAttackEvent PhysicalAttackEventData
  | DefenceEvent DefenceEventData
  | DeathEvent DeathEventData
  | GenerateEntityEvent GenerateEntityEventData
  | MoveEntityEvent MoveEntityEventData
  | MoveEntityDirectionEvent MoveEntityDirectionEventData
  | TickEvent TickEventData
  deriving (Show, Eq)

data IncomingDamageEventData =
  IncomingDamageEventData Int
  deriving (Show, Eq)

data OutgoingDamageEventData =
  OutgoingDamageEventData Int
  deriving (Show, Eq)

data PhysicalAttackEventData = PhysicalAttackEventData
  { attackerUUID :: UUID
  , defenderUUID :: UUID
  } deriving (Show, Eq)

data DefenceEventData =
  DefenceEventData Int
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

pushEvent
  :: (HasEventQueue s, MonadState s m)
  => Event -> m ()
pushEvent evt = eventQueue %= MkEventQueue . (flip (|>) evt) . unwrap
