module Zwerg where

import Zwerg.Component.Glyph
import Zwerg.Component.Position
import Zwerg.Data.Color (mkColor)
import Zwerg.Game
import Zwerg.UI
import Zwerg.UI.Backend.SDL
import Zwerg.UI.Backend.SDL.Init
import Zwerg.UI.Font
import Zwerg.UI.Input
import Zwerg.UI.Port
import Zwerg.Util

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, MonadState, runStateT)

import Control.Lens (makeClassy, use)
import qualified SDL

data ZwergState = ZwergState
    { _sdlContext :: BackendContext
    , _zsGameState  :: GameState
    , _zsUI         :: UserInterface
    }
makeClassy ''ZwergState

instance HasGameState ZwergState where
    gameState = zsGameState
instance HasUserInterface ZwergState where
    userInterface = zsUI
instance HasBackendContext ZwergState where
    backendContext = sdlContext

initZwergState :: ZwergState
initZwergState = ZwergState
    { _sdlContext  = uninitializedBackendContext
    , _zsGameState = emptyGameState
    , _zsUI        = initUI
    }

newtype Zwerg a = Zwerg (StateT ZwergState IO a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState ZwergState,
        MonadIO
    )

runZwerg :: Zwerg a -> IO (a, ZwergState)
runZwerg (Zwerg a) = runStateT a initZwergState

test :: Zwerg ()
test = initBackend >> drawZwergScreen >> mainLoop

mainLoop :: Zwerg ()
mainLoop = do
    whenJustM (fmap SDL.eventPayload <$> SDL.pollEvent) $ \case
        SDL.KeyboardEvent d -> whenJust (keyboardEventToKey d) processUserInput
        SDL.QuitEvent       -> quitZwerg
        _                   -> return ()

    mainLoop

processUserInput :: Key -> Zwerg ()
processUserInput k = do
    liftIO $ print k
    currentPort <- use (userInterface . port)
    case currentPort of
      MainMenu -> return()
      _ -> return ()

drawZwergScreen :: Zwerg ()
drawZwergScreen = do
    use (userInterface . port) >>= \case
      MainMenu -> return()
      _ -> return ()

    ren <- use (sdlContext . renderer)
    SDL.present ren

quitZwerg :: Zwerg ()
quitZwerg = do
    shutdownBackend

    -- forM_ [ (x',y') | x' <- [0..80], y' <- [0..25] ] $ \(x,y) ->
    --    blitGlyph (Glyph Normal 'A' $ mkColor 200 200 200) $ mkPosition (x,y)
    -- forM_ [ (x',y') | x' <- [81..105], y' <- [0..32] ] $ \(x,y) ->
    --    blitGlyph (Glyph Normal 'B' $ mkColor 200 200 200) $ mkPosition (x,y)
    -- forM_ [ (x',y') | x' <- [0..80], y' <- [26..32] ] $ \(x,y) ->
    --    blitGlyph (Glyph Normal 'C' $ mkColor 200 200 200) $ mkPosition (x,y)
