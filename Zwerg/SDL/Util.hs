module Zwerg.SDL.Util where

import Zwerg.Types
import qualified Graphics.UI.SDL as SDL

import Data.Word (Word16)
import Data.Int (Int32)

makeSDLColor :: Color -> Double -> SDL.Color
makeSDLColor White a = SDL.Color 240 240 240 (round $ 255 * a)
makeSDLColor Black a = SDL.Color 25 25 25 (round $ 255 * a)
makeSDLColor Red a = SDL.Color 222 84 197 (round $ 255 * a)
makeSDLColor _ a = SDL.Color 255 0 0 (round $ 255 * a)

convertSDLKeyCode :: Int32 -> KeyCode
convertSDLKeyCode SDL.SDLK_a = Letter 'a'
convertSDLKeyCode SDL.SDLK_b = Letter 'b'
convertSDLKeyCode SDL.SDLK_c = Letter 'c'
convertSDLKeyCode SDL.SDLK_d = Letter 'd'
convertSDLKeyCode SDL.SDLK_e = Letter 'e'
convertSDLKeyCode SDL.SDLK_f = Letter 'f'
convertSDLKeyCode SDL.SDLK_g = Letter 'g'
convertSDLKeyCode SDL.SDLK_h = Letter 'h'
convertSDLKeyCode SDL.SDLK_i = Letter 'i'
convertSDLKeyCode SDL.SDLK_j = Letter 'j'
convertSDLKeyCode SDL.SDLK_k = Letter 'k'
convertSDLKeyCode SDL.SDLK_h = Letter 'h'
convertSDLKeyCode SDL.SDLK_l = Letter 'l'
convertSDLKeyCode SDL.SDLK_m = Letter 'm'
convertSDLKeyCode SDL.SDLK_n = Letter 'n'
convertSDLKeyCode SDL.SDLK_o = Letter 'o'
convertSDLKeyCode SDL.SDLK_p = Letter 'p'
convertSDLKeyCode SDL.SDLK_q = Letter 'q'
convertSDLKeyCode SDL.SDLK_r = Letter 'r'
convertSDLKeyCode SDL.SDLK_s = Letter 's'
convertSDLKeyCode SDL.SDLK_t = Letter 't'
convertSDLKeyCode SDL.SDLK_u = Letter 'u'
convertSDLKeyCode SDL.SDLK_v = Letter 'v'
convertSDLKeyCode SDL.SDLK_w = Letter 'w'
convertSDLKeyCode SDL.SDLK_x = Letter 'x'
convertSDLKeyCode SDL.SDLK_y = Letter 'y'
convertSDLKeyCode SDL.SDLK_z = Letter 'z'
convertSDLKeyCode SDL.SDLK_RETURN = Return
convertSDLKeyCode sdlKC = error $ "No entry in convertSDLKeyCode for SDL.KeyCode: "
                                  ++ show sdlKC

convertSDLKeyMod :: Word16 -> KeyMod
convertSDLKeyMod _ = None
