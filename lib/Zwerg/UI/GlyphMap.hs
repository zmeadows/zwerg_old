module Zwerg.UI.GlyphMap where

import Zwerg.Prelude
import Zwerg.Component.Position (Position)
import Zwerg.Component.Glyph (Glyph)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, fromList, union)
import GHC.Generics (Generic)
import Data.Binary
import Control.Lens (Lens', iforM_)

newtype GlyphMap = MkGlyphMap (Map Position Glyph)
    deriving (Show, Read, Eq, Generic)

instance Binary GlyphMap

class HasGlyphMap s where
    glyphMap :: Lens' s GlyphMap
  
emptyGlyphMap :: GlyphMap
emptyGlyphMap = MkGlyphMap M.empty

mkGlyphMap :: [(Position, Glyph)] -> GlyphMap
mkGlyphMap = MkGlyphMap . M.fromList

mergeGlyphMaps :: GlyphMap -> GlyphMap -> GlyphMap
mergeGlyphMaps (MkGlyphMap gmUpdated) (MkGlyphMap gmMain) =
  MkGlyphMap (M.union gmUpdated gmMain)

forGlyphs :: Monad m
          => GlyphMap
          -> (Position -> Glyph -> m a)
          -> m ()
forGlyphs (MkGlyphMap gm) = iforM_ gm
