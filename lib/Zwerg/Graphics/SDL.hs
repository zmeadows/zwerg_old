module Zwerg.Graphics.SDL where

import Zwerg.Graphics.SDL.Core
import Zwerg.Data.Error
import Zwerg.Graphics.SDL.MainMenu
import Zwerg.Graphics.SDL.MainScreen
import Zwerg.UI.Port

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Except

import Control.Lens (Lens', makeClassy)

data ContextSDL = ContextSDL
    { _coreSDL           :: CoreContextSDL
    , _mainMenuContext   :: MainMenuContextSDL
    , _mainScreenContext :: MainScreenContextSDL
    }
makeClassy ''ContextSDL

sdl :: HasContextSDL s => Lens' s ContextSDL
sdl = contextSDL

instance HasCoreContextSDL ContextSDL where
    coreContextSDL = coreSDL
instance HasMainMenuContextSDL ContextSDL where
    mainMenuContextSDL = mainMenuContext
instance HasMainScreenContextSDL ContextSDL where
    mainScreenContextSDL = mainScreenContext

uninitializedContextSDL :: ContextSDL
uninitializedContextSDL = ContextSDL
    { _coreSDL           = unitializedCoreContextSDL
    , _mainMenuContext   = uninitializedMainMenuContextSDL
    , _mainScreenContext = uninitializedMainScreenContextSDL
    }

initSDL :: (HasCoreContextSDL s,
            HasMainScreenContextSDL s,
            HasMainMenuContextSDL s,
            MonadError ZError m,
            MonadState s m,
            MonadIO m)
        => m ()
initSDL = do
    initCoreSDL
    initializeMainScreenContextSDL
    let (MainMenu m) = initMainMenu in initializeMainMenuContextSDL m

shutdownSDL :: (HasCoreContextSDL s, MonadState s m, MonadIO m) => m ()
shutdownSDL = shutdownCoreSDL


