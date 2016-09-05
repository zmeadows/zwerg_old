module Zwerg.Behavior (
    EventGen(..),
    Behaviors(..),
    HasBehaviors(..),
    Behavior,
    emptyBehaviors
    ) where

import Zwerg.Data.RanGen (RanGen)
import Zwerg.Event (Event)
import Zwerg.Component (Components)
import Zwerg.Data.UUIDMap (UUIDMap)
import qualified Zwerg.Data.UUIDMap as UM

import Control.Lens (makeClassy, Lens')

newtype EventGen = MkEventGen
    { enact :: forall m. (Monad m)
            => RanGen
            -> Components
            -> m [Event]
    }

data Behaviors = Behaviors
    { _tick    :: UUIDMap EventGen
    , _onDeath :: UUIDMap EventGen
    }
makeClassy ''Behaviors

emptyBehaviors :: Behaviors
emptyBehaviors = Behaviors UM.empty UM.empty

type Behavior s = HasBehaviors s => Lens' s EventGen

instance Show Behaviors where
    show = const "<< Behaviors >>"
