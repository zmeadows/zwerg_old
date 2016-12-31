module Zwerg.UI.Backend.SDL where

import Zwerg.UI.Font
import Zwerg.UI.Input
import Zwerg.UI.Backend.SDL.Texture

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Foreign.Ptr (nullPtr)

import Control.Lens (makeClassy)
import qualified SDL
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Internal.Types as SDL.Internal

data BackendContext = BackendContext
    { _window        :: SDL.Window
    , _renderer      :: SDL.Renderer
    , _charTextures  :: CharTextureMap
    , _ttfFonts      :: HashMap FontType SDL.TTF.FFI.TTFFont
    }
makeClassy ''BackendContext

uninitializedBackendContext :: BackendContext
uninitializedBackendContext = BackendContext
    { _window        = SDL.Internal.Window nullPtr
    , _renderer      = SDL.Internal.Renderer nullPtr
    , _charTextures  = emptyCharTextureMap
    , _ttfFonts      = HM.empty
    }


keyboardEventToKey :: SDL.KeyboardEventData -> Maybe Key
keyboardEventToKey keyData =
    if (notRepeat && isKeyPress) 
       then convertSDLkey $ SDL.keyboardEventKeysym keyData
       else Nothing
  where notRepeat  = not $ SDL.keyboardEventRepeat keyData
        isKeyPress = (SDL.keyboardEventKeyMotion keyData) == SDL.Pressed

convertSDLkey :: SDL.Keysym -> Maybe Key
convertSDLkey (SDL.Keysym scanCode _ keyMod) = Just (None, Return)

