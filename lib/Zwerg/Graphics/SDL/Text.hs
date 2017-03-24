module Zwerg.Graphics.SDL.Text where

import Zwerg.Data.Color
import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.Util
import Zwerg.UI.Font
import Zwerg.Data.Error

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Data.Text (Text, unpack)
import Control.Monad.Except

import qualified SDL
import qualified SDL.TTF
import Control.Lens (use)

loadStringTexture :: ( HasCoreContextSDL s,
                       MonadState s m,
                       MonadError ZError m,
                       MonadIO m
                     )
                  => Text
                  -> FontType
                  -> Color
                  -> Color
                  -> m SDL.Texture
loadStringTexture str fontType fgColor bgColor = do
    font <- getTTFFont fontType
    strSurface <- SDL.TTF.renderUTF8Shaded font (unpack str)
        (colorToRawSDLColor fgColor) (colorToRawSDLColor bgColor)
    ren <- use renderer
    strTexture <- SDL.createTextureFromSurface ren strSurface
    SDL.freeSurface strSurface
    return strTexture
