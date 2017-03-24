module Zwerg.Data.Color (
    Color,
    mkColor,
    toRGB,
    toV3
    ) where

import Zwerg.Prelude

import Data.Binary
import Data.Word (Word8)
import GHC.Generics (Generic)
import Linear (V3(..))

newtype Color = MkColor (Word8,Word8,Word8)
    deriving (Show, Read, Eq, Generic)

instance Binary Color

{-# INLINABLE mkColor #-}
mkColor :: Word8 -> Word8 -> Word8 -> Color
mkColor r g b = MkColor (r,g,b)

{-# INLINABLE toRGB #-}
toRGB :: Color -> (Word8,Word8,Word8)
toRGB (MkColor c) = c

{-# INLINABLE toV3 #-}
toV3 :: Color -> V3 Word8
toV3 (MkColor (r,g,b)) = V3 r g b
