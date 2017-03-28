module Zwerg.Behavior
  ( EventGenerator(..)
  , Behaviors(..)
  , HasBehaviors(..)
  , Behavior
  , emptyBehaviors
  ) where

import Zwerg.Class
import Zwerg.Component (Components)
import Zwerg.Component.UUID
import Zwerg.Data.UUIDMap (UUIDMap)
import Zwerg.Event
import Zwerg.Prelude

import Control.Lens (makeClassy, Lens', (%=), use, at)
import Control.Monad.Random

newtype EventGenerator a = MkEventGenerator
  { react :: forall m. ( MonadState EventQueue m
                       , MonadReader Components m
                       , MonadRandom m
                       , EventData a
                       ) =>
                         a -> m ()
  }

data Behaviors = Behaviors
  { _tick :: UUIDMap (EventGenerator TickEventData)
  , _onDeath :: UUIDMap (EventGenerator DeathEventData)
  }

makeClassy ''Behaviors

emptyBehaviors :: Behaviors
emptyBehaviors = Behaviors zEmpty zEmpty

type Behavior s a = HasBehaviors s =>
                      Lens' s (UUIDMap (EventGenerator a))

addBehavior
  :: (HasBehaviors s, MonadState s m)
  => UUID -> Behavior s a -> EventGenerator a -> m ()
addBehavior uuid behavior gen = behavior %= zInsert uuid gen

getBehavior
  :: (HasBehaviors s, MonadState s m)
  => UUID -> Behavior s a -> m (Maybe (EventGenerator a))
getBehavior uuid behavior = use (behavior . at uuid)
