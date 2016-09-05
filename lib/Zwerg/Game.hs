module Zwerg.Game where

import Zwerg.Component    (Components, HasComponents(..), emptyComponents)
import Zwerg.Behavior     (Behaviors, HasBehaviors(..), emptyBehaviors)
import Zwerg.Data.UUIDGen (UUIDGen, HasUUIDGen(..), initUUIDGen)
import Zwerg.Log          (Log, HasLog(..), emptyLog)
import Zwerg.Data.Error   (Error)
import Zwerg.Data.RanGen  (RanGen)

import Control.Lens (makeClassy)

import Control.Monad.State  (MonadState, State)
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Random (MonadRandom, RandT)

data GameState = GameState
    { _gsComponents :: Components
    , _gsBehaviors  :: Behaviors
    , _gsUUIDGen    :: UUIDGen
    , _gsLog        :: Log
    } deriving (Show)
makeClassy ''GameState

emptyGameState :: GameState
emptyGameState = GameState
    { _gsComponents = emptyComponents
    , _gsBehaviors  = emptyBehaviors
    , _gsUUIDGen    = initUUIDGen
    , _gsLog        = emptyLog
    }

instance HasComponents GameState where
    components = gsComponents

instance HasBehaviors GameState where
    behaviors = gsBehaviors

instance HasUUIDGen GameState where
    uuidGen = gsUUIDGen

instance HasLog GameState where
    userLog = gsLog

newtype Game a = Game (ExceptT Error (RandT RanGen (State GameState)) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState GameState,
        MonadError Error,
        MonadRandom
    )
