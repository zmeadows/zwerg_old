module Zwerg.Graphics.SDL.MainMenu where

import Zwerg.Prelude
import Zwerg.Graphics.SDL.Core
import Zwerg.Graphics.SDL.Text
import Zwerg.Graphics.SDL.Util
import Zwerg.UI.Font
import Zwerg.UI.Menu
import Zwerg.Data.Color
import Zwerg.Data.Error
import Zwerg.Const
import Zwerg.Util

import Foreign.C.Types (CInt)
import Control.Monad (forM_, forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState)

import Control.Lens (makeClassy, (%=), use, assign, view)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import SDL (($=))
import SDL.Vect
import qualified Data.HashMap.Strict as HM
import qualified SDL

data MainMenuContextSDL = MainMenuContextSDL
    { _focusEntryTextures   :: HashMap Text SDL.Texture
    , _unfocusEntryTextures :: HashMap Text SDL.Texture
    , _mainMenuViewport     :: SDL.Rectangle CInt
    , _verticalSpacing      :: CInt
    } deriving (Eq)
makeClassy ''MainMenuContextSDL

uninitializedMainMenuContextSDL :: MainMenuContextSDL
uninitializedMainMenuContextSDL = MainMenuContextSDL
    { _focusEntryTextures   = HM.empty
    , _unfocusEntryTextures = HM.empty
    , _mainMenuViewport     = makeSDLRect (screenWidth / 3.0) (screenHeight / 3.0)
                                          (screenWidth / 3.0) (screenHeight / 3.0)
    , _verticalSpacing = 0
    }

initializeMainMenuContextSDL :: ( HasMainMenuContextSDL s,
                                  HasCoreContextSDL s,
                                  MonadError ZError m,
                                  MonadState s m,
                                  MonadIO m)
                             => TextMenu -> m ()
initializeMainMenuContextSDL menu =
    let fgFocus   = mkColor 255 100 100
        fgUnfocus = mkColor 255 255 255
        bg        = mkColor 25 25 25
     in do
        forM_ (getMenuLabels menu) $ \entry -> do
            focusTexture <- loadStringTexture entry Bold fgFocus bg
            unfocusTexture <- loadStringTexture entry Normal fgUnfocus bg
            (mainMenuContextSDL . focusEntryTextures) %= HM.insert entry focusTexture
            (mainMenuContextSDL . unfocusEntryTextures) %= HM.insert entry unfocusTexture
        computeEntryVerticalSpacing >>= assign verticalSpacing

computeEntryVerticalSpacing :: (HasMainMenuContextSDL s, MonadState s m, MonadIO m)
                            => m CInt
computeEntryVerticalSpacing = do
    textures <- HM.elems <$> use focusEntryTextures
    entryHeights <- forM textures $ \t -> SDL.queryTexture t >>= return . SDL.textureHeight
    return $ 4 + maximum entryHeights

drawMainMenu :: ( HasMainMenuContextSDL s,
                  HasCoreContextSDL s,
                  MonadError ZError m,
                  MonadState s m,
                  MonadIO m
                )
             => TextMenu -> m ()
drawMainMenu m = do
    ren <- use (core . renderer)
    vp <- use mainMenuViewport
    SDL.rendererViewport ren $= Just vp
    vs <- use verticalSpacing

    let entries = getMenuLabels m
        entryCoords = zip entries $ take (length entries) [0,vs..]
        SDL.Rectangle (P (V2 _ _)) (V2 mainMenuWidth _) = vp

    ft <- use focusEntryTextures
    ut <- use unfocusEntryTextures

    forM_ entryCoords $ \(entry,yCoord) -> do
        let txt = if (entry == (view label $ focus m))
            then HM.lookup entry ft
            else HM.lookup entry ut

        whenJustErr txt (ZError __FILE__ __LINE__ Fatal "Could not load main menu texture")
        $ \txt' -> do
            (w,h) <- getTextureDimensions txt'
            SDL.copy ren txt' Nothing $
                 Just $ SDL.Rectangle (P $ V2 ((mainMenuWidth - w) `div` 2) yCoord) (V2 w h)

    void $ SDL.rendererViewport ren $= Nothing
