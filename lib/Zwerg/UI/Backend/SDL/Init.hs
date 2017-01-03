module Zwerg.UI.Backend.SDL.Init where

import Zwerg.UI.Backend.SDL
import Zwerg.UI.Backend.SDL.Glyph
import Zwerg.UI.Font
import Zwerg.Util

import Control.Monad (forM_, unless)
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.HashMap.Strict as HM

import Linear (V2(..), V4(..))
import SDL (($=))
import qualified SDL
import qualified SDL.TTF
import Control.Lens (use, assign, (%=))

initBackend :: (HasBackendContext s, MonadState s m, MonadIO m) => m ()
initBackend = do
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

    _ <- SDL.TTF.init
    inited <- SDL.TTF.wasInit
    unless inited $ error "[Bug] Font system not initialized"

    let fonts =
            [ (Normal     , "fonts/Hack-Regular.ttf")
            , (Bold       , "fonts/Hack-Bold.ttf")
            , (Italic     , "fonts/Hack-Italic.ttf")
            , (BoldItalic , "fonts/Hack-BoldItalic.ttf")
            ]

    forM_ fonts $ \(ft, fp) -> do
        fullFontPath <- getAsset fp
        newTTFFont <- SDL.TTF.openFont fullFontPath 22
        ttfFonts %= HM.insert ft newTTFFont

    loadGlyphs
    use window >>= SDL.showWindow

shutdownBackend :: (HasBackendContext s, MonadState s m, MonadIO m) => m ()
shutdownBackend = do
    use window >>= SDL.destroyWindow
    use renderer >>= SDL.destroyRenderer
    SDL.quit
