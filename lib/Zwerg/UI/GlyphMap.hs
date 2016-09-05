module Zwerg.UI.GlyphMap where

import Zwerg.Component.Position (Position)
import Zwerg.Component.Glyph (Glyph)

import Data.HashMap.Strict (HashMap)

newtype GlyphMap = MkGlyphMap (HashMap Position Glyph)
    deriving (Show, Read, Eq)
