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
import Zwerg.Data.Error
import Zwerg.Data.RanGen
import Zwerg.Data.UUIDMap (UUIDMap)
import Zwerg.Event
import Zwerg.Prelude

import Control.Lens (makeClassy, Lens', (%=), use, at)
import Control.Monad.Random

newtype EventGenerator =
  EventGenerator (ExceptT ZError (RandT RanGen (ReaderT Components (State EventQueue))) ())

data Behaviors = Behaviors
  { _tick :: UUIDMap (TickEventData -> EventGenerator)
  , _onDeath :: UUIDMap EventGenerator
  , _onHit :: UUIDMap EventGenerator
  }

makeClassy ''Behaviors

emptyBehaviors :: Behaviors
emptyBehaviors = Behaviors zEmpty zEmpty zEmpty

type Behavior s a = (HasBehaviors s, EventData a) =>
                      Lens' s (UUIDMap (a -> EventGenerator))

addBehavior
  :: (HasBehaviors s, MonadState s m)
  => UUID -> Behavior s a -> (a -> EventGenerator) -> m ()
addBehavior uuid behavior gen = behavior %= zInsert uuid gen

getBehavior
  :: (HasBehaviors s, MonadState s m)
  => UUID -> Behavior s a -> m (Maybe (EventGenerator a))
getBehavior uuid behavior = use (behavior . at uuid)
