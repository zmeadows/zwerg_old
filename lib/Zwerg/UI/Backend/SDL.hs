module Zwerg.UI.Backend.SDL where

import qualified SDL
import qualified SDL.TTF
import SDL.TTF.FFI (TTFFont)
import qualified SDL.Raw as Raw

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad (unless, forM_, mapM_)
import Data.StateVar (($=))

import Foreign.Ptr (nullPtr)

import qualified SDL.Internal.Types as SDL.Internal

import Zwerg.Component.Glyph
import Zwerg.Component.Position
import Zwerg.Data.Color
import Zwerg.UI.Font
import Zwerg.Util
import Zwerg.Const (allChars)

import Zwerg.UI.Backend.SDL.Texture

import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))

import Data.Maybe (fromJust)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Control.Lens (
    makeClassy,
    assign,
    use,
    (%=)
    )

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

initBackend :: (HasBackendContext s, MonadState s m, MonadIO m) => m ()
initBackend = do
    SDL.initialize [SDL.InitVideo]

    SDL.createWindow "zwerg" SDL.defaultWindow
        { SDL.windowInitialSize = V2 1200 800
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
    -- SDL.TTF.quit

getUserInput :: (HasBackendContext s, MonadState s m, MonadIO m) => m ()
getUserInput = do
    e <- SDL.pollEvent
    case e of
      Just (SDL.Event _ (SDL.KeyboardEvent ked)) ->
          if not $ SDL.keyboardEventRepeat ked
             && (SDL.keyboardEventKeyMotion ked == SDL.Pressed)
          then liftIO $ print $ show $ SDL.keysymKeycode (SDL.keyboardEventKeysym ked)
          else getUserInput
      _ -> getUserInput

loadGlyphSurface :: (HasBackendContext s, MonadState s m, MonadIO m)
                 => FontType
                 -> Char
                 -> m ()
loadGlyphSurface ft ch = do
    font <- HM.lookup ft <$> use ttfFonts
    textSurface <- SDL.TTF.renderUTF8Shaded (fromJust font) [ch] (Raw.Color 255 255 255 0) (Raw.Color 25 25 25 0)
    ren <- use renderer
    textTexture <- SDL.createTextureFromSurface ren textSurface
    SDL.freeSurface textSurface
    SDL.queryTexture textTexture >>= \q -> liftIO $ print (SDL.textureWidth q, SDL.textureHeight q)
    charTextures %= addCharTexture ft ch textTexture

loadGlyphs :: (HasBackendContext s, MonadState s m, MonadIO m)
           => m ()
loadGlyphs = do
    mapM_ (loadGlyphSurface Normal) allChars
    mapM_ (loadGlyphSurface Bold) allChars
    mapM_ (loadGlyphSurface Italic) allChars
    mapM_ (loadGlyphSurface BoldItalic) allChars

glyphToTexture :: (HasBackendContext s, MonadState s m)
               => Glyph
               -> m SDL.Texture
glyphToTexture g = do
    t <- getCharTexture (fontType g) (char g) <$> use charTextures
    return $ fromJust t

blitGlyph :: (HasBackendContext s, MonadState s m, MonadIO m)
          => Glyph
          -> Position
          -> m ()
blitGlyph g pos = do
    let (x,y) = unPosition pos

    t <- glyphToTexture g
    SDL.textureColorMod t $= toV3 (color g)

    ren <- use renderer
    SDL.copy ren t Nothing $ Just $ SDL.Rectangle (P $ V2 (13* fromIntegral x) (27* fromIntegral y)) (V2 13 27)


