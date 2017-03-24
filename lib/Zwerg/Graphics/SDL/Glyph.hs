module Zwerg.Graphics.SDL.Glyph where

import Zwerg.Prelude
import Zwerg.UI.Font
import Zwerg.Util
import Zwerg.Graphics.SDL.Core
import Zwerg.Component.Glyph
import Zwerg.Data.Error

import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.State.Class (MonadState)
import Control.Monad (mapM_)

import qualified SDL
import qualified SDL.TTF
import qualified SDL.Raw as Raw
import Control.Lens (use)
import qualified Data.HashTable.IO as H

newtype CharTextureMap = MkCharTextureMap (H.CuckooHashTable (FontType,Char) SDL.Texture)

unitializedCharTextureMap :: CharTextureMap
unitializedCharTextureMap = MkCharTextureMap $ unsafePerformIO H.new

allChars :: [Char]
allChars = drop 33 $ take 127 [ (minBound :: Char) .. ]

initializeCharTextureMap :: ( HasCoreContextSDL s,
                              MonadState s m,
                              MonadError ZError m,
                              MonadIO m
                            )
                         => m CharTextureMap
initializeCharTextureMap = do
    ht <- liftIO (H.newSized $ 4 * (length allChars))
    let loadGlyphSurface ft ch = do
           font <- getTTFFont ft
           textSurface <- SDL.TTF.renderUTF8Shaded font [ch] (Raw.Color 255 255 255 0) (Raw.Color 0 0 0 255)
           ren <- use (core . renderer)
           textTexture <- SDL.createTextureFromSurface ren textSurface
           SDL.freeSurface textSurface
           liftIO $ H.insert ht (ft,ch) textTexture
    mapM_ (loadGlyphSurface Normal) allChars
    mapM_ (loadGlyphSurface Bold) allChars
    mapM_ (loadGlyphSurface Italic) allChars
    mapM_ (loadGlyphSurface BoldItalic) allChars
    return $ MkCharTextureMap ht

glyphToRawTexture :: (MonadError ZError m, MonadIO m)
                  => Glyph
                  -> CharTextureMap
                  -> m SDL.Texture
glyphToRawTexture glyph (MkCharTextureMap charMap) = do
    tex <- liftIO $ H.lookup charMap (fontType glyph, char glyph)
    whenJustErr tex (ZError __FILE__ __LINE__ Fatal "Could not load raw glyph texture") return

