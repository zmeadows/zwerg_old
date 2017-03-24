{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Zwerg.Graphics.SDL.MainScreen where

import Zwerg.Prelude
import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.Glyph
import Zwerg.Graphics.SDL.Util
import Zwerg.Component.Glyph
import Zwerg.Component.Position
import Zwerg.Data.Color
import Zwerg.Data.Error
import Zwerg.Const
import Zwerg.UI.GlyphMap
 
import Foreign.C.Types (CInt)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
 
import Control.Lens (makeClassy, use, assign)
import SDL (($=))
import SDL.Vect
import qualified SDL

data MainScreenContextSDL = MainScreenContextSDL
    { _charTextures   :: CharTextureMap
    , _mainViewport    :: SDL.Rectangle CInt
    , _statusViewport :: SDL.Rectangle CInt
    , _logViewport    :: SDL.Rectangle CInt
    }
makeClassy ''MainScreenContextSDL

pixelGap :: Double
pixelGap = 4.0

mainW, mainH :: Double
mainW = 0.75
mainH = 0.75

statusW, statusH :: Double
statusW = 1.0 - mainW
statusH = 1.0

logW, logH :: Double
logW = mainW
logH = 1.0 - mainH
  

uninitializedMainScreenContextSDL :: MainScreenContextSDL
uninitializedMainScreenContextSDL = MainScreenContextSDL
    { _charTextures   = unitializedCharTextureMap
    , _mainViewport    = makeSDLRect pixelGap pixelGap
                                    (screenWidth * mainW - pixelGap/2.0)
                                    (screenHeight * mainH - pixelGap/2.0)
    , _statusViewport = makeSDLRect (screenWidth * mainW + 3*pixelGap/2.0) pixelGap
                                    (screenWidth * statusW - 5.0 * pixelGap / 2.0)
                                    (screenHeight * statusH - 2*pixelGap)
    , _logViewport    = makeSDLRect pixelGap (screenHeight * mainH + 3*pixelGap/2.0)
                                    (screenWidth * logW - pixelGap/2.0)
                                    (screenHeight * logH - 5.0 * pixelGap / 2.0)
    }

initializeMainScreenContextSDL :: ( HasMainScreenContextSDL s,
                                    HasCoreContextSDL s,
                                    MonadError ZError m,
                                    MonadState s m,
                                    MonadIO m)
                               => m ()
initializeMainScreenContextSDL = do
  initializeCharTextureMap >>= assign charTextures
  return ()

blitGlyph :: ( HasCoreContextSDL s,
               HasMainScreenContextSDL s,
               MonadError ZError m,
               MonadState s m,
               MonadIO m
             )
          => Position
          -> Glyph
          -> m ()
blitGlyph pos g = do
    let (x,y) = unPosition pos
    t <- use charTextures >>= glyphToRawTexture g
    SDL.textureColorMod t $= toV3 (color g)
    ren <- use (core . renderer)
    SDL.copy ren t Nothing $ Just $ SDL.Rectangle
      (P $ V2 (13* fromIntegral x) (27* fromIntegral y)) (V2 13 27)


drawMainScreen :: ( HasMainScreenContextSDL s,
                    HasCoreContextSDL s,
                    MonadState s m,
                    MonadError ZError m,
                    MonadIO m)
               => GlyphMap -> m ()
drawMainScreen gm = do
  ren <- use (core . renderer)

  mainVP   <- use mainViewport
  statusVP <- use statusViewport
  logVP    <- use logViewport

  SDL.rendererViewport ren $= Nothing
  SDL.rendererDrawColor ren $= zwergBkgColor
  SDL.clear ren

  SDL.rendererDrawColor ren $= V4 255 100 100 maxBound
  SDL.drawRect ren $ Just mainVP

  SDL.rendererDrawColor ren $= V4 100 255 100 maxBound
  SDL.drawRect ren $ Just statusVP

  SDL.rendererDrawColor ren $= V4 100 100 255 maxBound
  SDL.drawRect ren $ Just logVP

  SDL.rendererViewport ren $= Just mainVP
  forGlyphs gm blitGlyph

  return ()

  

  -- forM_ [ (x',y') | x' <- [0..80], y' <- [0..25] ] $ \(x,y) ->
  --         blitGlyph (Glyph Normal 'A' $ mkColor 200 200 200) $ mkPosition (x,y)
  -- forM_ [ (x',y') | x' <- [81..105], y' <- [0..32] ] $ \(x,y) ->
  --         blitGlyph (Glyph Normal 'B' $ mkColor 200 200 200) $ mkPosition (x,y)
  -- forM_ [ (x',y') | x' <- [0..80], y' <- [26..32] ] $ \(x,y) ->
  --         blitGlyph (Glyph Normal 'C' $ mkColor 200 200 200) $ mkPosition (x,y)
