module Zwerg.Graphics.SDL.MainMenu where

import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.Text
import Zwerg.UI.Font
import Zwerg.UI.Menu
import Zwerg.Data.Color

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)

import qualified SDL
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Lens (makeClassy, (%=))

data MainMenuContextSDL = MainMenuContextSDL
    { _focusEntryTextures   :: HashMap Text SDL.Texture
    , _unfocusEntryTextures :: HashMap Text SDL.Texture
    } deriving (Eq)
makeClassy ''MainMenuContextSDL

uninitializedMainMenuContextSDL :: MainMenuContextSDL
uninitializedMainMenuContextSDL = MainMenuContextSDL HM.empty HM.empty

initializeMainMenuContextSDL :: (HasMainMenuContextSDL s, HasCoreContextSDL s, MonadState s m, MonadIO m)
                             => Menu Text -> m ()
initializeMainMenuContextSDL menu =
    forM_ (menuToList menu) $ \entry -> do
        focusTexture <- loadStringTexture entry Bold fgFocus bg
        unfocusTexture <- loadStringTexture entry Normal fgUnfocus bg
        (mainMenuContextSDL . focusEntryTextures) %= HM.insert entry focusTexture
        (mainMenuContextSDL . unfocusEntryTextures) %= HM.insert entry unfocusTexture
  where fgFocus   = mkColor 255 255 255
        fgUnfocus = mkColor 255 100 100
        bg        = mkColor 25 25 25

