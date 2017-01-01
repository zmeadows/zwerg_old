module Zwerg (zwerg) where

import Zwerg.Component.Glyph
import Zwerg.Component.Position
import Zwerg.Data.Color (mkColor)
import Zwerg.Game
import Zwerg.UI
import Zwerg.UI.Backend.SDL
import Zwerg.UI.Backend.SDL.Init
import Zwerg.UI.Font
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, MonadState, runStateT)

import Control.Lens (makeClassy, use, (.=))
import qualified SDL

data ZwergState = ZwergState
	{ _sdlContext :: BackendContext
	, _zsGameState	:: GameState
	, _zsUI			:: UserInterface
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
	, _zsUI		   = initUI
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

zwerg = runZwerg test

mainLoop :: Zwerg ()
mainLoop = do
	currentPort <- use (userInterface . port)
	whenJustM (fmap SDL.eventPayload <$> SDL.pollEvent) $ \case
		SDL.KeyboardEvent d -> do
	  		whenJust (keyboardEventToKey d) (processUserInput currentPort)
	 		newPort <- use (userInterface . port)
			drawZwergScreen newPort
		SDL.QuitEvent		-> quitZwerg
		_					-> return ()

	mainLoop

processUserInput :: Port -> Key -> Zwerg ()
processUserInput (MainMenu m) (None, KeyChar 'j') =
	userInterface . port .= (MainMenu $ next m)

processUserInput (MainMenu m) (None, KeyChar 'k') =
	userInterface . port .= (MainMenu $ prev m)

processUserInput _ _ = return ()

drawZwergScreen :: Port -> Zwerg ()
drawZwergScreen MainMenu = do
	ren <- use (sdlContext . renderer)
	SDL.present ren

drawZwergScreen _ = return ()

quitZwerg :: Zwerg ()
quitZwerg = shutdownBackend

	-- forM_ [ (x',y') | x' <- [0..80], y' <- [0..25] ] $ \(x,y) ->
	--	  blitGlyph (Glyph Normal 'A' $ mkColor 200 200 200) $ mkPosition (x,y)
	-- forM_ [ (x',y') | x' <- [81..105], y' <- [0..32] ] $ \(x,y) ->
	--	  blitGlyph (Glyph Normal 'B' $ mkColor 200 200 200) $ mkPosition (x,y)
	-- forM_ [ (x',y') | x' <- [0..80], y' <- [26..32] ] $ \(x,y) ->
	--	  blitGlyph (Glyph Normal 'C' $ mkColor 200 200 200) $ mkPosition (x,y)
