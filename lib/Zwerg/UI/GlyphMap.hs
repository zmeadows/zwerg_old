module Zwerg.UI.GlyphMap where

import Zwerg.Component.Position (Position)
import Zwerg.Component.Glyph (Glyph)

import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Data.Binary

newtype GlyphMap = MkGlyphMap (Map Position Glyph)
    deriving (Show, Read, Eq, Generic)

instance Binary GlyphMap
