module Zwerg.UI.Backend.SDL where

import Zwerg.Data.Color
import Zwerg.UI.Font
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Backend.SDL.Texture

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Foreign.Ptr (nullPtr)
import Data.Text (Text, unpack)

import Control.Lens (makeClassy, use, (%=))
import SDL.TTF.FFI (TTFFont)
import qualified SDL
import qualified SDL.TTF
import qualified SDL.Internal.Types as SDL.Internal
import qualified SDL.Raw as Raw

data MainMenuContextSDL = MainMenuContextSDL
    { _focusEntryTextures   :: HashMap Text SDL.Texture
    , _unfocusEntryTextures :: HashMap Text SDL.Texture
    } deriving (Eq)
makeClassy ''MainMenuContextSDL

data BackendContext = BackendContext
    { _window           :: SDL.Window
    , _renderer         :: SDL.Renderer
    , _charTextures     :: CharTextureMap
    , _ttfFonts         :: HashMap FontType SDL.TTF.FFI.TTFFont
    , _mainMenuContext  :: MainMenuContextSDL
    }
makeClassy ''BackendContext

instance HasMainMenuContextSDL BackendContext where
    mainMenuContextSDL = mainMenuContext

uninitializedBackendContext :: BackendContext
uninitializedBackendContext = BackendContext
    { _window           = SDL.Internal.Window nullPtr
    , _renderer         = SDL.Internal.Renderer nullPtr
    , _charTextures     = emptyCharTextureMap
    , _ttfFonts         = HM.empty
    , _mainMenuContext  = MainMenuContextSDL HM.empty HM.empty
    }

getFont :: (HasBackendContext s, MonadState s m)
        => FontType -> m SDL.TTF.FFI.TTFFont
getFont ft = fromJust . HM.lookup ft <$> use ttfFonts

keyboardEventToKey :: SDL.KeyboardEventData -> Maybe Key
keyboardEventToKey keyData =
    if notRepeat && isKeyPress
       then convertSDLkey $ SDL.keyboardEventKeysym keyData
       else Nothing
  where notRepeat  = not $ SDL.keyboardEventRepeat keyData
        isKeyPress = SDL.keyboardEventKeyMotion keyData == SDL.Pressed

convertSDLkey :: SDL.Keysym -> Maybe Key
convertSDLkey (SDL.Keysym SDL.ScancodeH _ _) = Just (None, KeyChar 'h')
convertSDLkey (SDL.Keysym SDL.ScancodeJ _ _) = Just (None, KeyChar 'j')
convertSDLkey (SDL.Keysym SDL.ScancodeK _ _) = Just (None, KeyChar 'k')
convertSDLkey (SDL.Keysym SDL.ScancodeL _ _) = Just (None, KeyChar 'l')
convertSDLkey _ = Nothing

colorToRawSDLColor :: Color -> Raw.Color
colorToRawSDLColor col = let (r,g,b) = toRGB col in Raw.Color r g b 0

makeMainMenuContextSDL :: (HasMainMenuContextSDL s, HasBackendContext s,
                          MonadState s m, MonadIO m)
                       => Menu Text -> m ()
makeMainMenuContextSDL menu =
    forM_ (menuToList menu) $ \entry -> do
        focusTexture <- loadStringTexture entry Bold fgFocus bg
        unfocusTexture <- loadStringTexture entry Normal fgUnfocus bg
        (mainMenuContextSDL . focusEntryTextures) %= HM.insert entry focusTexture
        (mainMenuContextSDL . unfocusEntryTextures) %= HM.insert entry unfocusTexture
  where fgFocus   = mkColor 255 255 255
        fgUnfocus = mkColor 255 100 100
        bg        = mkColor 25 25 25

loadStringTexture :: (HasBackendContext s, MonadState s m, MonadIO m)
                  => Text
                 -> FontType
                 -> Color
                 -> Color
                 -> m SDL.Texture
loadStringTexture str fontType fgColor bgColor = do
    font <- getFont fontType
    strSurface <- SDL.TTF.renderUTF8Shaded font (unpack str)
        (colorToRawSDLColor fgColor) (colorToRawSDLColor bgColor)
    ren <- use renderer
    strTexture <- SDL.createTextureFromSurface ren strSurface
    SDL.freeSurface strSurface
    return strTexture
