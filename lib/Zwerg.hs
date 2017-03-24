module Zwerg (zwerg) where

import Zwerg.Data.Error
import Zwerg.Data.RanGen
import Zwerg.Game
import Zwerg.Graphics.SDL
import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.MainMenu
import Zwerg.Graphics.SDL.MainScreen
import Zwerg.Graphics.SDL.Util
import Zwerg.Prelude
import Zwerg.UI.GlyphMap
import Zwerg.UI.Port
import Zwerg.Util

import Control.Lens (makeClassy, use, assign)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified SDL

data ZwergState = ZwergState
    { _sdlContext  :: ContextSDL
    , _zsGameState :: GameState
    , _ranGen      :: RanGen
    , _quitting    :: Bool
    }
makeClassy ''ZwergState

instance HasGameState ZwergState where
    gameState = zsGameState
instance HasContextSDL ZwergState where
    contextSDL = sdlContext
instance HasCoreContextSDL ZwergState where
    coreContextSDL = sdlContext . core
instance HasMainMenuContextSDL ZwergState where
    mainMenuContextSDL = sdlContext . mainMenuContext
instance HasMainScreenContextSDL ZwergState where
    mainScreenContextSDL = sdlContext . mainScreenContext

initZwergState :: ZwergState
initZwergState = ZwergState
    { _sdlContext  = uninitializedContextSDL
    , _zsGameState = emptyGameState
    , _ranGen      = pureRanGen 0
    , _quitting    = False
    }

newtype Zwerg a = Zwerg (ExceptT ZError (StateT ZwergState IO) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState ZwergState,
        MonadError ZError,
        MonadIO
    )

runZwerg :: Zwerg a -> IO (Either ZError a, ZwergState)
runZwerg (Zwerg a) = runStateT (runExceptT a) initZwergState

test :: Zwerg ()
test = do
    newPureRanGen >>= assign ranGen
    initSDL
    currentPort <- use (gameState . port)
    drawZwergScreen currentPort
    mainLoop

zwerg :: IO (Either ZError (), ZwergState)
zwerg = runZwerg test

mainLoop :: Zwerg ()
mainLoop = do
    whenJustM (fmap SDL.eventPayload <$> SDL.pollEvent) $ \case
        SDL.KeyboardEvent ked -> whenJust (keyboardEventToKey ked) $ \keycode -> do
            st <- use gameState
            gen <- use ranGen
            let (st', err, gen') = runGame (processUserInput keycode) gen st
            assign gameState st'
            assign ranGen gen'
            currentPort <- use (gameState . port)
            drawZwergScreen currentPort
            whenJust err $ \err' -> do
              liftIO $ print err'
              assign quitting True
        SDL.QuitEvent       -> assign quitting True
        _                   -> return ()

    use quitting >>= \q -> if q then quitZwerg else mainLoop

drawZwergScreen :: Port -> Zwerg ()
drawZwergScreen (MainMenu m) = do
    ren <- use (sdl . renderer)
    SDL.clear ren
    drawMainMenu m
    SDL.present ren

drawZwergScreen MainScreen = do
    ren <- use (sdl . renderer)
    gm <- use (gameState . glyphMap)
    drawMainScreen gm
    SDL.present ren

drawZwergScreen _ = return ()

quitZwerg :: Zwerg ()
quitZwerg = do
  shutdownSDL
