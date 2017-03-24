module Zwerg.Graphics.SDL.Util where

import Zwerg.Prelude
import Zwerg.Data.Color
import Zwerg.UI.Input

import Data.Word (Word8)
import qualified SDL
import qualified SDL.Raw as Raw
import SDL.Vect
import Foreign.C.Types (CInt)

zwergBkgColor :: V4 Word8
zwergBkgColor = V4 25 25 25 maxBound

makeSDLRect :: (RealFrac a, RealFrac b, RealFrac c, RealFrac d)
            => a -> b -> c -> d -> SDL.Rectangle CInt
makeSDLRect x y w h = SDL.Rectangle (P (V2 x' y')) (V2 w' h')
  where x' = fromIntegral (round x :: Int) :: CInt
        y' = fromIntegral (round y :: Int) :: CInt
        w' = fromIntegral (round w :: Int) :: CInt
        h' = fromIntegral (round h :: Int) :: CInt

colorToRawSDLColor :: Color -> Raw.Color
colorToRawSDLColor col = let (r,g,b) = toRGB col in Raw.Color r g b 0

convertScancode :: SDL.Scancode -> Maybe KeyCode
convertScancode SDL.ScancodeA = Just $ KeyChar 'a'
convertScancode SDL.ScancodeB = Just $ KeyChar 'b'
convertScancode SDL.ScancodeC = Just $ KeyChar 'c'
convertScancode SDL.ScancodeD = Just $ KeyChar 'd'
convertScancode SDL.ScancodeE = Just $ KeyChar 'e'
convertScancode SDL.ScancodeF = Just $ KeyChar 'f'
convertScancode SDL.ScancodeG = Just $ KeyChar 'g'
convertScancode SDL.ScancodeH = Just $ KeyChar 'h'
convertScancode SDL.ScancodeI = Just $ KeyChar 'i'
convertScancode SDL.ScancodeJ = Just $ KeyChar 'j'
convertScancode SDL.ScancodeK = Just $ KeyChar 'k'
convertScancode SDL.ScancodeL = Just $ KeyChar 'l'
convertScancode SDL.ScancodeM = Just $ KeyChar 'm'
convertScancode SDL.ScancodeN = Just $ KeyChar 'n'
convertScancode SDL.ScancodeO = Just $ KeyChar 'o'
convertScancode SDL.ScancodeP = Just $ KeyChar 'p'
convertScancode SDL.ScancodeQ = Just $ KeyChar 'q'
convertScancode SDL.ScancodeR = Just $ KeyChar 'r'
convertScancode SDL.ScancodeS = Just $ KeyChar 's'
convertScancode SDL.ScancodeT = Just $ KeyChar 't'
convertScancode SDL.ScancodeU = Just $ KeyChar 'u'
convertScancode SDL.ScancodeV = Just $ KeyChar 'v'
convertScancode SDL.ScancodeW = Just $ KeyChar 'w'
convertScancode SDL.ScancodeX = Just $ KeyChar 'x'
convertScancode SDL.ScancodeY = Just $ KeyChar 'y'
convertScancode SDL.ScancodeZ = Just $ KeyChar 'z'
convertScancode SDL.Scancode1 = Just $ KeyChar '1'
convertScancode SDL.Scancode2 = Just $ KeyChar '2'
convertScancode SDL.Scancode3 = Just $ KeyChar '3'
convertScancode SDL.Scancode4 = Just $ KeyChar '4'
convertScancode SDL.Scancode5 = Just $ KeyChar '5'
convertScancode SDL.Scancode6 = Just $ KeyChar '6'
convertScancode SDL.Scancode7 = Just $ KeyChar '7'
convertScancode SDL.Scancode8 = Just $ KeyChar '8'
convertScancode SDL.Scancode9 = Just $ KeyChar '9'
convertScancode SDL.Scancode0 = Just $ KeyChar '0'
convertScancode SDL.ScancodeReturn       = Just Return
convertScancode SDL.ScancodeEscape       = Just Escape
convertScancode SDL.ScancodeBackspace    = Just Backspace
convertScancode SDL.ScancodeTab          = Just Tab
convertScancode SDL.ScancodeSpace        = Just $ KeyChar ' '
convertScancode SDL.ScancodeMinus        = Just $ KeyChar '-'
convertScancode SDL.ScancodeEquals       = Just $ KeyChar '='
convertScancode SDL.ScancodeLeftBracket  = Just $ KeyChar '{'
convertScancode SDL.ScancodeRightBracket = Just $ KeyChar '}'
convertScancode SDL.ScancodeComma        = Just $ KeyChar ','
convertScancode SDL.ScancodePeriod       = Just $ KeyChar '.'
convertScancode _ = Nothing

keyboardEventToKey :: SDL.KeyboardEventData -> Maybe KeyCode
keyboardEventToKey keyData =
    if notRepeat && isKeyPress
       then convertScancode $ (SDL.keysymScancode . SDL.keyboardEventKeysym) keyData
       else Nothing
  where notRepeat  = not $ SDL.keyboardEventRepeat keyData
        isKeyPress = SDL.keyboardEventKeyMotion keyData == SDL.Pressed
