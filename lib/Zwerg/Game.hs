module Zwerg.Game where

import Zwerg.Behavior     (Behaviors, HasBehaviors(..), emptyBehaviors)
import Zwerg.Component    (Components, HasComponents(..), emptyComponents)
import Zwerg.Data.Error   (Error)
import Zwerg.Data.RanGen  (RanGen)
import Zwerg.Data.UUIDGen (UUIDGen, HasUUIDGen(..), initUUIDGen)
import Zwerg.Log          (Log, HasLog(..), emptyLog)
import Zwerg.UI.Port
import Zwerg.UI.Input
import Zwerg.UI.Menu

import Control.Lens (makeClassy, use, (.=))
import Control.Monad.Except (MonadError, ExceptT)
import Control.Monad.Random (MonadRandom, RandT)
import Control.Monad.State  (MonadState, State)

data GameState = GameState
    { _gsComponents :: Components
    , _gsBehaviors  :: Behaviors
    , _gsUUIDGen    :: UUIDGen
    , _gsLog        :: Log
    , _gsPort       :: Port
    } deriving (Show)
makeClassy ''GameState

emptyGameState :: GameState
emptyGameState = GameState
    { _gsComponents = emptyComponents
    , _gsBehaviors  = emptyBehaviors
    , _gsUUIDGen    = initUUIDGen
    , _gsLog        = emptyLog
    , _gsPort       = initMainMenu
    }

instance HasComponents GameState where
    components = gsComponents
instance HasBehaviors GameState where
    behaviors = gsBehaviors
instance HasUUIDGen GameState where
    uuidGen = gsUUIDGen
instance HasLog GameState where
    userLog = gsLog
instance HasPort GameState where
    port = gsPort

newtype Game a = Game (ExceptT Error (RandT RanGen (State GameState)) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState GameState,
        MonadError Error,
        MonadRandom
    )

processUserInput :: KeyCode -> Game ()
processUserInput k = do
    currentPort <- use port
    processUserInput' currentPort k

processUserInput' :: Port -> KeyCode -> Game ()
processUserInput' (MainMenu m) (KeyChar 'j') =
    port .= (MainMenu $ next m)

processUserInput' (MainMenu m) (KeyChar 'k') =
    port .= (MainMenu $ prev m)

processUserInput' _ _ = return ()
