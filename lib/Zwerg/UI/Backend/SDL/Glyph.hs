module Zwerg.UI.Backend.SDL.Glyph where

import Zwerg.UI.Backend.SDL 
import Zwerg.UI.Backend.SDL.Texture
import Zwerg.UI.Font
import Zwerg.Component.Glyph
import Zwerg.Component.Position
import Zwerg.Data.Color
import Zwerg.Const (allChars)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad (mapM_)
import Data.StateVar (($=))
import Data.Maybe
import qualified Data.HashMap.Strict as HM

import qualified SDL
import qualified SDL.TTF
import qualified SDL.Raw as Raw
import Linear (V2(..), V4(..))
import Linear.Affine (Point(..))
import Control.Lens (use, (%=))

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

