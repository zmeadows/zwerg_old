module Zwerg where


import Zwerg.UI
import Zwerg.UI.Backend.SDL
import Zwerg.UI.Font
import Zwerg.Game

import qualified SDL

import Zwerg.Data.Color (mkColor)
import Zwerg.Component.Glyph
import Zwerg.Component.Position

import Control.Concurrent (threadDelay)
import Control.Monad.State (StateT, MonadState, runStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Lens (makeClassy, use)

import Control.Monad (forM_)

data ZwergState = ZwergState
    { _sdlContext  :: BackendContext
    , _zsGameState :: GameState
    , _zsUI        :: UserInterface
    }
makeClassy ''ZwergState

initZwergState :: ZwergState
initZwergState = ZwergState
    { _sdlContext  = uninitializedBackendContext
    , _zsGameState = emptyGameState
    , _zsUI        = initUI
    }

instance HasGameState ZwergState where
    gameState = zsGameState

instance HasUserInterface ZwergState where
    userInterface = zsUI

instance HasBackendContext ZwergState where
    backendContext = sdlContext

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
test = do
    initBackend

    forM_ [ (x',y') | x' <- [0..60], y' <- [0..20] ] $ \(x,y) ->
       blitGlyph (Glyph Normal '.' $ mkColor 200 200 200) $ mkPosition (x,y)

    ren <- use (sdlContext . renderer)
    SDL.present ren
    liftIO $ threadDelay 10000000
    shutdownBackend

