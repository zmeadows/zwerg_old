module Zwerg.Graphics.SDL.MainScreen where

import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.Glyph
import Zwerg.UI.Font
import Zwerg.Component.Glyph
import Zwerg.Component.Position
-- import Zwerg.UI.Menu
import Zwerg.Data.Color
import Zwerg.Const
-- 
import Foreign.C.Types (CInt)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)
-- import Data.Maybe (fromJust)
-- 
import Control.Lens (makeClassy, (%=), use, assign)
-- import Data.HashMap.Strict (HashMap)
-- import Data.Text (Text)
import SDL (($=))
import SDL.Vect
-- import qualified Data.HashMap.Strict as HM
import qualified SDL

data MainScreenContextSDL = MainScreenContextSDL
    { _charTextures   :: CharTextureMap
    , _mapViewport    :: SDL.Rectangle CInt
    , _statusViewport :: SDL.Rectangle CInt
    , _logViewport    :: SDL.Rectangle CInt
    }
makeClassy ''MainScreenContextSDL

uninitializedMainScreenContextSDL :: MainScreenContextSDL
uninitializedMainScreenContextSDL = MainScreenContextSDL
    { _charTextures   = unitializedCharTextureMap
    , _mapViewport    = SDL.Rectangle
                                (P (V2 (screenWidth `div` 3) (screenHeight `div` 3)))
                                (V2 (screenWidth `div` 3) (screenHeight `div` 3))
    , _statusViewport = SDL.Rectangle
                                (P (V2 (screenWidth `div` 3) (screenHeight `div` 3)))
                                (V2 (screenWidth `div` 3) (screenHeight `div` 3))
    , _logViewport    = SDL.Rectangle
                                (P (V2 (screenWidth `div` 3) (screenHeight `div` 3)))
                                (V2 (screenWidth `div` 3) (screenHeight `div` 3))
    }

initializeMainScreenContextSDL :: (HasMainScreenContextSDL s, HasCoreContextSDL s, MonadState s m, MonadIO m)
                               => m ()
initializeMainScreenContextSDL = do
  initializeCharTextureMap >>= assign charTextures
  return ()

blitGlyph :: (HasCoreContextSDL s, HasMainScreenContextSDL s, MonadState s m, MonadIO m)
          => Glyph
          -> Position
          -> m ()
blitGlyph g pos = do
    let (x,y) = unPosition pos
    t <- use charTextures >>= glyphToRawTexture g
    SDL.textureColorMod t $= toV3 (color g)
    ren <- use (core . renderer)
    SDL.copy ren t Nothing $ Just $ SDL.Rectangle
      (P $ V2 (13* fromIntegral x) (27* fromIntegral y)) (V2 13 27)


drawMainScreen :: (HasMainScreenContextSDL s, HasCoreContextSDL s, MonadState s m, MonadIO m)
               => m ()
drawMainScreen = do
  forM_ [ (x',y') | x' <- [0..80], y' <- [0..25] ] $ \(x,y) ->
          blitGlyph (Glyph Normal 'A' $ mkColor 200 200 200) $ mkPosition (x,y)
  forM_ [ (x',y') | x' <- [81..105], y' <- [0..32] ] $ \(x,y) ->
          blitGlyph (Glyph Normal 'B' $ mkColor 200 200 200) $ mkPosition (x,y)
  forM_ [ (x',y') | x' <- [0..80], y' <- [26..32] ] $ \(x,y) ->
          blitGlyph (Glyph Normal 'C' $ mkColor 200 200 200) $ mkPosition (x,y)
