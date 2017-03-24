module Zwerg.Game where

import Zwerg.Behavior     (Behaviors, HasBehaviors(..), emptyBehaviors)
import Zwerg.Component    
import Zwerg.Data.Error   (ZError)
import Zwerg.Data.RanGen  (RanGen)
import qualified Zwerg.Data.Direction as Direction
import Zwerg.Log          (Log, HasLog(..), emptyLog)
import Zwerg.UI.Port
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.GlyphMap
import Zwerg.Generator
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Player.TestPlayer

import Control.Lens (makeClassy, use, (.=), (%=), view)
import Control.Monad.Random (runRandT, RandT, MonadRandom)

data GameState = GameState
    { _gsComponents :: Components
    , _gsBehaviors  :: Behaviors
    , _gsUUIDGen    :: UUIDGen
    , _gsLog        :: Log
    , _gsPort       :: Port
    , _gsGlyphMap   :: GlyphMap
    }
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
instance HasGlyphMap GameState where
    glyphMap = gsGlyphMap

newtype Game a = Game (ExceptT ZError (RandT RanGen (State GameState)) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState GameState,
        MonadError ZError,
        MonadRandom
    )

runGame :: Game () -> RanGen -> GameState -> (GameState, Maybe ZError, RanGen)
runGame (Game a) gen st = 
    let ((e,gen'),st') = runIdentity (runStateT (runRandT (runExceptT a) gen) st)
        in case e of
            Left err -> (st', Just err, gen')
            Right () -> (st', Nothing, gen')

generateGame :: Game ()
generateGame = do
  lid <- generate testSquareGenerator
  generate $ testPlayerGenerator lid

processUserInput :: KeyCode -> Game ()
processUserInput k = do
    currentPort <- use port
    processUserInput' currentPort k
    newPort <- use port
    when (newPort == MainScreen) updateGlyphMap

processUserInput' :: Port -> KeyCode -> Game ()
processUserInput' (MainMenu m) (KeyChar 'j') =
    port .= (MainMenu $ next m)

processUserInput' (MainMenu m) (KeyChar 'k') =
    port .= (MainMenu $ prev m)

processUserInput' (MainMenu m) Return =
    case (view label $ focus m) of
      "new game" -> generateGame >> port .= MainScreen
      "exit"     -> port .= ExitScreen
      _          -> return ()

processUserInput' MainScreen (KeyChar 'h') = moveEntityDirection playerUUID Direction.Left
processUserInput' MainScreen (KeyChar 'j') = moveEntityDirection playerUUID Direction.Down
processUserInput' MainScreen (KeyChar 'k') = moveEntityDirection playerUUID Direction.Up
processUserInput' MainScreen (KeyChar 'l') = moveEntityDirection playerUUID Direction.Right

processUserInput' _ _ = return ()

updateGlyphMap :: Game ()
updateGlyphMap = do
  updatedGlyphs <- getGlyphMapUpdates
  glyphMap %= mergeGlyphMaps updatedGlyphs
