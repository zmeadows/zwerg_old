{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Zwerg.SDL where

import Zwerg.Core
import Zwerg.Entity
import Zwerg.Layer.Simple
import Zwerg.Port
import Zwerg.Entity.Player
import Zwerg.Types

import Prelude hiding ((.))
import Control.Category ((.))
import Control.Monad.State.Strict hiding (get, gets)
import Data.Label
import Data.Label.Monadic
import System.Random.Mersenne.Pure64

import Control.Monad.IO.Class (liftIO)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Graphics.UI.SDL.TTF.Types
import Graphics.UI.SDL.TTF.FFI
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL as SDL

import Foreign.C.String (newCString)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
import Foreign (peek,poke,alloca,with,maybePeek,nullPtr)
import Data.Maybe (fromJust)

import Data.Char (chr)

fclabels [d|
    data ZWERGSDLState = ZWERGSDLState {
        zwergState     :: ZWERGState,
        window         :: SDL.Window,
        windowDims     :: (Int,Int),
        font           :: TTFFont,
        renderer       :: SDL.Renderer,
        chDims         :: (CInt, CInt),
        charTextureMap :: Map Char SDL.Texture,
        mainSubWin     :: SDL.Rect,
        statSubWin     :: SDL.Rect,
        msgSubWin      :: SDL.Rect
    } deriving (Show)
  |]

type ZWERGSDL a = StateT ZWERGSDLState IO a

emptyRect :: SDL.Rect
emptyRect = SDL.Rect 0 0 0 0

initTestEntities :: System Port
initTestEntities = do
    layerUUID <- makeSimpleLayer 105 15
    void $ makePlayer "Bob" (5,5) 10 layerUUID
    return OverWorld

initZwergSDLState :: ZWERGSDLState
initZwergSDLState = ZWERGSDLState (initZwergState emptyEntities (pureMT 0))
                        nullPtr (1280,720) nullPtr nullPtr (0,0) M.empty
                        emptyRect emptyRect emptyRect


initSubWindows :: ZWERGSDL ()
initSubWindows = do
    mainSubWin =: SDL.Rect 0 0 960 540
    statSubWin =: SDL.Rect 961 0 320 720
    msgSubWin  =: SDL.Rect 0 541 960 180

renderGlyphMap :: ZWERGSDL ()
renderGlyphMap = do
     ren <- gets renderer
     msw <- gets mainSubWin
     gm <- gets (glyphMap . zwergState)
     (chW, chH) <- gets chDims
     switchSubWindow ren msw
     mapM_ (\( (x,y), (ch,_,_)) -> drawChar ch (chW * fromIntegral x, chH * fromIntegral y)) $ M.toList gm
     SDL.renderPresent ren

initGraphics :: ZWERGSDL ()
initGraphics = do
    SDL.init SDL.SDL_INIT_VIDEO

    title <- liftIO $ newCString "zwerg"
    newWin <- SDL.createWindow title
                 SDL.SDL_WINDOWPOS_UNDEFINED
                 SDL.SDL_WINDOWPOS_UNDEFINED
                 1280 720
                 SDL.SDL_WINDOW_SHOWN
    window =: newWin

    newRen <- SDL.createRenderer newWin (-1) 0
    renderer =: newRen

    SDL.setRenderDrawBlendMode newRen SDL.SDL_BLENDMODE_BLEND


    liftIO TTF.init
    newFont <- liftIO $ TTF.openFont "/Users/zac/Code/mine/zwerg/fonts/Envy Code R.ttf" 17
    liftIO $ TTF.setFontHinting newFont TTFHLight
    font =: newFont

    initTextureMap
    initSubWindows

    SDL.setRenderDrawColor newRen 25 25 25 0
    SDL.renderClear newRen
    SDL.renderPresent newRen
    SDL.setRenderDrawColor newRen 25 25 25 0
    SDL.renderClear newRen
    SDL.renderPresent newRen



closeGraphics :: ZWERGSDL ()
closeGraphics = do
    gets renderer >>= SDL.destroyRenderer
    gets window >>= SDL.destroyWindow
    gets font >>= liftIO . TTF.closeFont
    liftIO TTF.quit
    SDL.quit

initTextureMap :: ZWERGSDL ()
initTextureMap = do
    let chars = map chr [32..126]
    f <- gets font
    (w,h) <- liftIO $ TTF.sizeText f "@"
    liftIO $ print w
    chDims =: (fromIntegral w, fromIntegral h)
    chtxts <- mapM charToTexture chars
    charTextureMap =: M.fromList (zip chars chtxts)

playZWERG :: ZWERGSDL ()
playZWERG = do
    initGraphics
    zs <- gets zwergState
    zwergState =. execState (modifyEntities initTestEntities)
    renderGlyphMap
    handleEvents
    closeGraphics

charToTexture :: Char -> ZWERGSDL SDL.Texture
charToTexture ch = do
    f <- gets font
    ren <- gets renderer
    chSurface <- liftIO $ TTF.renderTextShaded f [ch]
                            (SDL.Color 240 240 240 0)
                            (SDL.Color 25 25 25 0)
    chTexture <- SDL.createTextureFromSurface ren chSurface
    SDL.freeSurface chSurface
    return chTexture

switchSubWindow :: SDL.Renderer -> SDL.Rect -> ZWERGSDL ()
switchSubWindow renderer rect = void $ liftIO $ with rect $ \rectPtr ->
                                    SDL.renderSetViewport renderer rectPtr

drawChar :: Char -> (CInt,CInt) -> ZWERGSDL ()
drawChar ch (x,y) = do
    texture <- liftM (fromJust . M.lookup ch) $ gets charTextureMap
    (w,h) <- gets chDims
    ren <- gets renderer
    let loc = SDL.Rect x y w h
    void $ liftIO $ with loc $ \loc' ->
             SDL.renderCopy ren texture nullPtr loc'

drawString :: SDL.Rect -> String -> (CInt,CInt) -> ZWERGSDL ()
drawString subwin str (x,y) = do
    fnt <- gets font
    ren <- gets renderer
    switchSubWindow ren subwin
    strSurf <- liftIO $ TTF.renderTextShaded fnt str
                            (SDL.Color 240 240 240 0)
                            (SDL.Color 25 25 25 0)
    strTexture <- SDL.createTextureFromSurface ren strSurf
    (w,h) <- liftIO $ TTF.sizeText fnt str
    SDL.freeSurface strSurf
    let loc = SDL.Rect x y (fromIntegral w) (fromIntegral h)
    void $ liftIO $ with loc $ \loc' ->
             SDL.renderCopy ren strTexture nullPtr loc'

getEvent :: IO (Maybe SDL.Event)
getEvent = alloca $ \ptr -> do
  status <- SDL.pollEvent ptr
  if status == 1
    then maybePeek peek ptr
    else return Nothing

handleEvents :: ZWERGSDL ()
handleEvents = do
  mbEvent <- liftIO getEvent
  case mbEvent of
    Just (SDL.KeyboardEvent { keyboardEventKeysym = ks, keyboardEventState = SDL.SDL_PRESSED } )
        -> handleKeyEvent (keyCodeToChar ks) >> renderGlyphMap >> handleEvents
    Just (SDL.QuitEvent _ _) -> return ()
    _ -> handleEvents

handleKeyEvent :: Char -> ZWERGSDL ()
handleKeyEvent ch = do
    p <- gets (port . zwergState)
    zwergState =. execState (modifyEntities $ processInput p (None,Letter ch) )

keyCodeToChar :: SDL.Keysym -> Char
keyCodeToChar SDL.Keysym {keysymKeycode = SDL.SDLK_j} = 'j'
keyCodeToChar SDL.Keysym {keysymKeycode = SDL.SDLK_k} = 'k'
keyCodeToChar SDL.Keysym {keysymKeycode = SDL.SDLK_h} = 'h'
keyCodeToChar SDL.Keysym {keysymKeycode = SDL.SDLK_l} = 'l'
keyCodeToChar _ = ' '
