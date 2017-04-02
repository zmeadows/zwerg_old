module Zwerg.UI.GlyphMap where

import Zwerg.Component.Position
import Zwerg.Prelude
import Zwerg.Util

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
       (empty, fromList, union, toList)

newtype GlyphMap =
  MkGlyphMap (Map Position Glyph)
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

-- forGlyphs
--   :: Monad m
--   => GlyphMap -> (Position -> Glyph -> m a) -> m ()
-- forGlyphs (MkGlyphMap gm) = forM_ gm
glyphMapToRows :: GlyphMap -> [[Glyph]]
glyphMapToRows (MkGlyphMap gm) =
  let glyphList = M.toList gm
      sortedGlyphList = sortBy brickCmpPos glyphList
  in chunksOf mapWidthINT (map snd sortedGlyphList)

brickCmpPos :: (Position, Glyph) -> (Position, Glyph) -> Ordering
brickCmpPos (p1, _) (p2, _) =
  let (x1, y1) = unPosition p1
      (x2, y2) = unPosition p2
  in if | y1 > y2 -> GT
        | x1 > x2 -> GT
        | otherwise -> LT
