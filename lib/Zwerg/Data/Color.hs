module Zwerg.Data.Color (
    Color,
    mkColor,
    toRGB,
    toV3
    ) where

import Data.Colour
import Data.Colour.SRGB
import Data.Word (Word8)

import Linear (V3(..))

newtype Color = MkColor (Colour Float)
    deriving (Show, Read, Eq)

{-# INLINABLE mkColor #-}
mkColor :: Word8 -> Word8 -> Word8 -> Color
mkColor r g b = MkColor $ sRGB24 r g b

{-# INLINABLE toRGB #-}
toRGB :: Color -> (Word8,Word8,Word8)
toRGB (MkColor c) = go $ toSRGB24 c
    where go rgb = (channelRed rgb, channelGreen rgb, channelBlue rgb)

{-# INLINABLE toV3 #-}
toV3 :: Color -> V3 Word8
toV3 (MkColor c) = go $ toSRGB24 c
    where go rgb = V3 (channelRed rgb) (channelGreen rgb) (channelBlue rgb)
