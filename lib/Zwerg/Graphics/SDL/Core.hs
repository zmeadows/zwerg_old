module Zwerg.Graphics.SDL.Core where

import Zwerg.UI.Font
import Zwerg.Util

import Data.Maybe (fromJust)

import Control.Lens (Lens', makeClassy, assign, use, (%=))
import Control.Monad
import SDL (($=))
import qualified SDL
import qualified SDL.TTF
import qualified SDL.Internal.Types as SDL.Internal
import SDL.TTF.FFI (TTFFont)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
import Foreign.Ptr (nullPtr)
import Linear (V2(..), V4(..))

type FontSDL = SDL.TTF.FFI.TTFFont

data CoreContextSDL = CoreContextSDL
    { _window   :: SDL.Window
    , _renderer :: SDL.Renderer
    , _fonts    :: HashMap FontType FontSDL
    }
makeClassy ''CoreContextSDL

core :: (HasCoreContextSDL s) => Lens' s CoreContextSDL
core = coreContextSDL

unitializedCoreContextSDL :: CoreContextSDL
unitializedCoreContextSDL = CoreContextSDL 
    { _window   = SDL.Internal.Window nullPtr
    , _renderer = SDL.Internal.Renderer nullPtr
    , _fonts    = HM.empty
    }

initCoreSDL :: (HasCoreContextSDL s, MonadState s m, MonadIO m)
            => m ()
initCoreSDL = do
    SDL.initialize [SDL.InitVideo]

    SDL.createWindow "zwerg" SDL.defaultWindow
        { SDL.windowInitialSize = V2 1400 900
        , SDL.windowBorder = True
        } >>= assign window

    SDL.HintRenderScaleQuality $= SDL.ScaleLinear

    let rconf = SDL.RendererConfig SDL.AcceleratedRenderer False

    use window >>= \w -> SDL.createRenderer w (-1) rconf >>= assign renderer
    ren <- use renderer
    SDL.rendererDrawColor ren $= V4 25 25 25 maxBound
    SDL.clear ren

    void SDL.TTF.init
    inited <- SDL.TTF.wasInit
    -- TODO: replace with zwerg error
    unless inited $ error "[Bug] Font system not initialized"

    let fontPaths =
            [ (Normal     , "fonts/Hack-Regular.ttf")
            , (Bold       , "fonts/Hack-Bold.ttf")
            , (Italic     , "fonts/Hack-Italic.ttf")
            , (BoldItalic , "fonts/Hack-BoldItalic.ttf")
            ]

    forM_ fontPaths $ \(ft, fp) -> do
        fullFontPath <- getAsset fp
        newTTFFont <- SDL.TTF.openFont fullFontPath 22
        fonts %= HM.insert ft newTTFFont

    use window >>= SDL.showWindow

shutdownCoreSDL :: (HasCoreContextSDL s, MonadState s m, MonadIO m) => m ()
shutdownCoreSDL = do
    use window >>= SDL.destroyWindow
    use renderer >>= SDL.destroyRenderer
    SDL.quit

getTTFFont :: (HasCoreContextSDL s, MonadState s m)
           => FontType 
           -> m FontSDL
getTTFFont ft = fromJust <$> HM.lookup ft <$> use fonts
