module Zwerg (zwerg) where

import Zwerg.Game
import Zwerg.Graphics.SDL
import Zwerg.Graphics.SDL.Core
import Zwerg.UI.Port
import Zwerg.Util

import Control.Monad.State (StateT, MonadState, runStateT)
import Control.Monad.IO.Class (MonadIO)

import Control.Lens (makeClassy, use)
import qualified SDL

data ZwergState = ZwergState
    { _sdlContext  :: ContextSDL
    , _zsGameState :: GameState
    }
makeClassy ''ZwergState

instance HasGameState ZwergState where
    gameState = zsGameState
instance HasContextSDL ZwergState where
    contextSDL = sdlContext
instance HasCoreContextSDL ZwergState where
    coreContextSDL = sdlContext . core

initZwergState :: ZwergState
initZwergState = ZwergState
    { _sdlContext  = uninitializedContextSDL
    , _zsGameState = emptyGameState
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
test = initSDL >> mainLoop

zwerg :: IO ((), ZwergState)
zwerg = runZwerg test

mainLoop :: Zwerg ()
mainLoop = do
    currentPort <- use (gameState . port)
    whenJustM (fmap SDL.eventPayload <$> SDL.pollEvent) $ \case
        SDL.KeyboardEvent _ -> drawZwergScreen currentPort
            -- whenJust (keyboardEventToKey d) $ \key -> do
            --     newPort <- use (gameState . port)
            --     drawZwergScreen newPort
        SDL.QuitEvent       -> quitZwerg
        _                   -> return ()

    mainLoop

drawZwergScreen :: Port -> Zwerg ()
drawZwergScreen (MainMenu _) = do
    --let focusedItem = focus m
    ren <- use (sdl . renderer)
    SDL.clear ren
    SDL.present ren

drawZwergScreen _ = return ()

quitZwerg :: Zwerg ()
quitZwerg = shutdownSDL
