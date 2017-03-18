module Zwerg.UI.GlyphMap where

import Zwerg.Component.Position (Position)
import Zwerg.Component.Glyph (Glyph)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, fromList)
import GHC.Generics (Generic)
import Data.Binary

newtype GlyphMap = MkGlyphMap (Map Position Glyph)
    deriving (Show, Read, Eq, Generic)

instance Binary GlyphMap

emptyGlyphMap :: GlyphMap
emptyGlyphMap = MkGlyphMap M.empty

mkGlyphMap :: [(Position, Glyph)] -> GlyphMap
mkGlyphMap = MkGlyphMap . M.fromList
