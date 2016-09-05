module Zwerg.UI.Backend.SDL.Texture where

import qualified SDL

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Zwerg.UI.Font

newtype CharTextureMap = MkCharTextureMap
    (HashMap FontType (HashMap Char SDL.Texture))

emptyCharTextureMap :: CharTextureMap
emptyCharTextureMap = MkCharTextureMap $
    HM.fromList $ map (,HM.empty) [(minBound :: FontType) ..]

{-# INLINABLE addCharTexture #-}
addCharTexture :: FontType -> Char -> SDL.Texture -> CharTextureMap -> CharTextureMap
addCharTexture ft ch tx (MkCharTextureMap m) = MkCharTextureMap $
    HM.adjust (HM.insert ch tx) ft m

{-# INLINABLE getCharTexture #-}
getCharTexture :: FontType -> Char -> CharTextureMap -> Maybe SDL.Texture
getCharTexture ft ch (MkCharTextureMap m) =
    HM.lookup ft m >>= HM.lookup ch
