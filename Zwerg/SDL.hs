{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell, TypeOperators #-}
module Zwerg.SDL where

import Zwerg.Core
-- import Zwerg.Entity
-- import Zwerg.Layer.Simple
import Zwerg.Port
-- import Zwerg.Entity.Player
import Zwerg.Types
import Zwerg.SDL.Util

import Prelude hiding ((.))
import Control.Category ((.))
import Data.Label (fclabels, (:->))
import Data.Label.Monadic ((=:), (=.), gets, puts)

-- import System.Random.Mersenne.Pure64 (pureMT)
import Control.Monad.State.Strict (StateT, execState)
import Control.Monad (when, replicateM_, liftM, void)
import Control.Monad.IO.Class (liftIO)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.TTF.FFI
import qualified Graphics.UI.SDL.TTF as TTF
import qualified Graphics.UI.SDL as SDL

import Graphics.UI.SDL.Image

import Foreign.C.String (newCString)
import Foreign.C.Types (CInt)
import Foreign (Ptr, nullPtr, with, alloca, maybePeek, peek)

import Data.Text (Text)
import qualified Data.Text as T (unpack)

-- import Data.Char (chr)

import System.Exit (exitFailure, exitSuccess)

fclabels [d|
    data ZWERGSDLState = ZWERGSDLState {
        zwergState       :: ZWERGState,
        window           :: SDL.Window,
        renderer         :: SDL.Renderer,
        fontRegular      :: TTFFont,
        fontBold         :: TTFFont,
        fontItalic       :: TTFFont,
        fontBoldItalic   :: TTFFont,
        glyphTextureDims :: (Int, Int),
        glyphTextureMap  :: Map Glyph SDL.Texture,
        logoTexture      :: SDL.Texture
    } deriving (Show)
  |]

type ZWERGSDL a = StateT ZWERGSDLState IO a

_INITZWERGSDLSTATE :: ZWERGSDLState
_INITZWERGSDLSTATE = ZWERGSDLState _INITZWERGSTATE nullPtr nullPtr nullPtr
                                   nullPtr nullPtr nullPtr (0,0) M.empty nullPtr

_SCREENWIDTH, _SCREENHEIGHT :: CInt
_SCREENWIDTH  = 1280
_SCREENHEIGHT = 720

_WHOLESCREEN, _MAINSUBWIN, _STATSUBWIN, _MSGSUBWIN :: SDL.Rect
_WHOLESCREEN = SDL.Rect 0   0   _SCREENWIDTH _SCREENHEIGHT
_MAINSUBWIN  = SDL.Rect 0   0   960 540
_STATSUBWIN  = SDL.Rect 961 0   320 720
_MSGSUBWIN   = SDL.Rect 0   541 960 180

_REGFONTPATH, _BOLDFONTPATH, _ITALICFONTPATH, _BOLDITALICFONTPATH :: String
_REGFONTPATH        = "/Users/zac/Code/mine/zwerg/assets/fonts/LiberationMono-Regular.ttf"
_BOLDFONTPATH       = "/Users/zac/Code/mine/zwerg/assets/fonts/LiberationMono-Bold.ttf"
_ITALICFONTPATH     = "/Users/zac/Code/mine/zwerg/assets/fonts/LiberationMono-Italic.ttf"
_BOLDITALICFONTPATH = "/Users/zac/Code/mine/zwerg/assets/fonts/LiberationMono-BoldItalic.ttf"

checkRC :: String -> CInt -> ZWERGSDL ()
checkRC errmsg rc = liftIO $ when (rc /= 0) $ do
    putStrLn $ errmsg ++ " ERROR CODE: " ++ show rc
    exitFailure

checkPTR :: Ptr a -> String -> ZWERGSDL ()
checkPTR ptr errmsg = liftIO $ when (ptr == nullPtr) $ do
    putStrLn errmsg
    exitFailure

initGraphics :: ZWERGSDL ()
initGraphics = do
    SDL.init SDL.SDL_INIT_VIDEO
        >>= checkRC "failed to initialize SDL."
    liftIO $ imgInit [InitPNG]

    title <- liftIO $ newCString "zwerg"

    _window <- SDL.createWindow title
                    SDL.SDL_WINDOWPOS_UNDEFINED
                    SDL.SDL_WINDOWPOS_UNDEFINED
                    _SCREENWIDTH _SCREENHEIGHT
                    SDL.SDL_WINDOW_SHOWN
    checkPTR _window "failed to initialize SDL window."
    window =: _window

    _renderer <- SDL.createRenderer _window (-1) 0
    checkPTR _renderer "failed to initialize SDL renderer for window."
    renderer =: _renderer

    SDL.setRenderDrawBlendMode _renderer SDL.SDL_BLENDMODE_BLEND
        >>= checkRC "failed to set blend mode for rgba rendering"

    replicateM_ 2 $ do
         SDL.setRenderDrawColor _renderer 25 25 25 0
            >>= checkRC "failed to set draw color"
         SDL.renderClear _renderer
            >>= checkRC "failed to clear screen"
         SDL.renderPresent _renderer

closeGraphics :: ZWERGSDL ()
closeGraphics = do
    gets renderer >>= SDL.destroyRenderer
    gets window >>= SDL.destroyWindow
    liftIO imgQuit
    SDL.quit
    liftIO exitSuccess

closeFonts :: ZWERGSDL ()
closeFonts = do
    gets fontRegular    >>= liftIO . TTF.closeFont
    gets fontBold       >>= liftIO . TTF.closeFont
    gets fontItalic     >>= liftIO . TTF.closeFont
    gets fontBoldItalic >>= liftIO . TTF.closeFont
    liftIO TTF.quit

initFonts :: ZWERGSDL ()
initFonts = do
    liftIO TTF.init >>= checkRC "failed to initialize SDL_TTF"
    mapM_ initFont [(fontRegular, _REGFONTPATH),
                    (fontBold, _BOLDFONTPATH),
                    (fontItalic, _ITALICFONTPATH),
                    (fontBoldItalic, _BOLDITALICFONTPATH)]

    rf <- gets fontRegular
    liftIO (TTF.sizeText rf "@") >>= puts glyphTextureDims

initFont :: (ZWERGSDLState :-> TTFFont, String) -> ZWERGSDL ()
initFont (fontLens, fontPath) = do
    _font <- liftIO $ TTF.openFont fontPath 15
    checkPTR _font $ "SDL_TTF failed to initialize font: " ++ fontPath
    liftIO $ TTF.setFontHinting _font TTFHLight
    fontLens =: _font

getFont :: FontType -> ZWERGSDL TTFFont
getFont ft = gets $ case ft of FontRegular -> fontRegular
                               FontBold -> fontBold
                               FontItalic -> fontItalic
                               FontBoldItalic -> fontBoldItalic

glyphToTexture :: Glyph -> ZWERGSDL SDL.Texture
glyphToTexture (Glyph ch ft fg bg fa ba) = do
    ren <- gets renderer
    font <- getFont ft
    glyphSurface <- liftIO $ TTF.renderTextShaded font [ch]
                                (makeSDLColor fg fa) (makeSDLColor bg ba)
    glyphTexture <- SDL.createTextureFromSurface ren glyphSurface
    SDL.freeSurface glyphSurface
    return glyphTexture

getGlyphTexture :: Glyph -> ZWERGSDL SDL.Texture
getGlyphTexture glyph = do
    gt <- liftM (M.lookup glyph) $ gets glyphTextureMap
    maybe (do
        gt' <- glyphToTexture glyph
        glyphTextureMap =. M.insert glyph gt'
        return gt') return gt

drawText :: SDL.Rect -> FontType -> Color -> Color -> (CInt,CInt) -> Text -> ZWERGSDL ()
drawText sw ft fg bg (x,y) str = do
    font <- getFont ft
    ren <- gets renderer
    switchSubWindow ren sw
    textSurface <- liftIO $ TTF.renderTextShaded font (T.unpack str)
                                (makeSDLColor fg 0) (makeSDLColor bg 0)
    textTexture <- SDL.createTextureFromSurface ren textSurface
    SDL.freeSurface textSurface
    (w,h) <- liftIO $ TTF.sizeText font (T.unpack str)
    let loc = SDL.Rect x y (fromIntegral w) (fromIntegral h)
    void $ liftIO $ with loc $ \loc' ->
                SDL.renderCopy ren textTexture nullPtr loc'

switchSubWindow :: SDL.Renderer -> SDL.Rect -> ZWERGSDL ()
switchSubWindow ren rect = liftIO (with rect $ \rectPtr ->
    SDL.renderSetViewport ren rectPtr) >>= checkRC "failed to switch SDL viewport"

drawScreen :: Port -> ZWERGSDL ()

drawScreen (MainMenu entries focus) = do
    ren <- gets renderer
    Right logoTexture <- liftIO $ imgLoadTexture ren "/Users/zac/Code/mine/zwerg/assets/images/logo.png"
    let loc = SDL.Rect 340 30 600 500

    SDL.setRenderDrawColor ren 240 240 240 0
       >>= checkRC "failed to set draw color"
    SDL.renderClear ren
       >>= checkRC "failed to clear screen"

    let indToColor i
            | i == focus = Red
            | otherwise = Black

    liftIO $ with loc $ \loc' -> SDL.renderCopy ren logoTexture nullPtr loc'
    drawText _WHOLESCREEN FontBold (indToColor 0) White (605,500) "new game"
    drawText _WHOLESCREEN FontRegular (indToColor 1) White (605,520) "options"
    drawText _WHOLESCREEN FontRegular (indToColor 2) White (605,540) "about"
    drawText _WHOLESCREEN FontRegular (indToColor 3) White (605,560) "quit"
    SDL.renderPresent ren

drawScreen OverWorld = do
    ren <- gets renderer
    SDL.setRenderDrawColor ren 240 240 240 0
       >>= checkRC "failed to set draw color"
    SDL.renderClear ren
       >>= checkRC "failed to clear screen"
    SDL.renderPresent ren

drawScreen _ = return ()

getEvent :: IO (Maybe SDL.Event)
getEvent = alloca $ \ptr -> do
  status <- SDL.pollEvent ptr
  if status == 1
    then maybePeek peek ptr
    else return Nothing

eventLoop :: ZWERGSDL ()
eventLoop = do
    gets (port . zwergState) >>= drawScreen
    mbEvent <- liftIO getEvent
    case mbEvent of
      Just ev -> handleEvent ev >> eventLoop
      Nothing -> eventLoop

handleEvent :: SDL.Event -> ZWERGSDL ()

handleEvent ke@SDL.KeyboardEvent{ SDL.keyboardEventState = SDL.SDL_PRESSED }  = do
   let ks = SDL.keyboardEventKeysym ke
       kc = convertSDLKeyCode $ SDL.keysymKeycode ks
       km = convertSDLKeyMod $ SDL.keysymMod ks
   liftIO $ print (kc,km)
   p <- gets (port . zwergState)
   zwergState =. execState (modifyEntities $ processInput p (km,kc))

handleEvent SDL.QuitEvent{} = closeGraphics

handleEvent _ = return ()

-- renderGlyphMap :: ZWERGSDL ()
-- renderGlyphMap = do
--      ren <- gets renderer
--      gm <- gets (glyphMap . zwergState)
--      (chW, chH) <- gets chDims
--      switchSubWindow ren _MAINSUBWIN
--      mapM_ (\( (x,y), (ch,_,_)) -> drawChar ch (chW * fromIntegral x, chH * fromIntegral y)) $ M.toList gm
--      SDL.renderPresent ren

-- drawChar :: Char -> (CInt,CInt) -> ZWERGSDL ()
-- drawChar ch (x,y) = do
--     texture <- liftM (fromJust . M.lookup ch) $ gets charTextureMap
--     (w,h) <- gets chDims
--     ren <- gets renderer
--     let loc = SDL.Rect x y w h
--     void $ liftIO $ with loc $ \loc' ->
--              SDL.renderCopy ren texture nullPtr loc'
--
--
--
--
-- colorToRGB :: Zwerg.Types.Color -> RGB
-- colorToRGB White = (240,240,240)
-- colorToRGB Black = (240,240,240)
-- colorToRGB _ = (255,0,0)
