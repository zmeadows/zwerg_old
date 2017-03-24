module Zwerg.Behavior (
    EventGenerator(..),
    Behaviors(..),
    HasBehaviors(..),
    Behavior,
    emptyBehaviors
    ) where

import Zwerg.Prelude
import Zwerg.Class
import Zwerg.Event (Event)
import Zwerg.Component (Components)
import Zwerg.Data.UUIDMap (UUIDMap)

import Control.Lens (makeClassy, Lens')
import Control.Monad.Random       

newtype EventGenerator = MkEventGenerator {
  enact :: forall m. (
      MonadReader Components m,
      MonadRandom m
      ) => Event -> m [Event]
    }

data Behaviors = Behaviors
    { _tick    :: UUIDMap EventGenerator
    , _onDeath :: UUIDMap EventGenerator
    }
makeClassy ''Behaviors

emptyBehaviors :: Behaviors
emptyBehaviors = Behaviors zEmpty zEmpty

type Behavior s = HasBehaviors s => Lens' s EventGenerator

