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
import Zwerg.UI.GlyphMap

import Data.Functor.Identity
import Control.Lens (makeClassy, use, (.=))
import Control.Monad.Except
import Control.Monad.Random (runRandT, RandT, MonadRandom)
import Control.Monad.State.Strict
import Control.Monad.State.Class (MonadState)

data GameState = GameState
    { _gsComponents :: Components
    , _gsBehaviors  :: Behaviors
    , _gsUUIDGen    :: UUIDGen
    , _gsLog        :: Log
    , _gsPort       :: Port
    , _gsGlyphMap   :: GlyphMap
    } deriving (Show)
makeClassy ''GameState

emptyGameState :: GameState
emptyGameState = GameState
    { _gsComponents = emptyComponents
    , _gsBehaviors  = emptyBehaviors
    , _gsUUIDGen    = initUUIDGen
    , _gsLog        = emptyLog
    , _gsPort       = initMainMenu
    , _gsGlyphMap   = emptyGlyphMap
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

runGame :: Game () -> RanGen -> GameState -> (GameState, Maybe Error, RanGen)
runGame (Game a) gen st = 
    let ((e,gen'),st') = runIdentity (runStateT (runRandT (runExceptT a) gen) st)
        in case e of
            Left err -> (st', Just err, gen')
            Right () -> (st', Nothing, gen')

processUserInput :: KeyCode -> Game ()
processUserInput k = do
    currentPort <- use port
    processUserInput' currentPort k

processUserInput' :: Port -> KeyCode -> Game ()
processUserInput' (MainMenu m) (KeyChar 'j') =
    port .= (MainMenu $ next m)

processUserInput' (MainMenu m) (KeyChar 'k') =
    port .= (MainMenu $ prev m)

processUserInput' (MainMenu m) Return =
    case (label $ focus m) of
      "new game" -> port .= MainScreen
      "exit"     -> port .= ExitScreen
      _          -> return ()

processUserInput' _ _ = return ()
