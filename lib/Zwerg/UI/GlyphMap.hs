module Zwerg.UI.GlyphMap where

import Zwerg.Component.Position
import Zwerg.Prelude
import Zwerg.Util

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
       (empty, fromList, union, toList, map)

newtype GlyphMap =
  MkGlyphMap (Map Position (Glyph, Bool))
  deriving (Show, Read, Eq, Generic)

instance Binary GlyphMap

class HasGlyphMap s where
  glyphMap :: Lens' s GlyphMap

emptyGlyphMap :: GlyphMap
emptyGlyphMap = MkGlyphMap M.empty

mkGlyphMap :: [(Position, (Glyph, Bool))] -> GlyphMap
mkGlyphMap = MkGlyphMap . M.fromList

blankGlyphMap :: MonadError ZError m => m GlyphMap
blankGlyphMap = do
 let xs = [0 .. mapWidthINT - 1]
     ys = [0 .. mapHeightINT - 1]
     emptyGlyph = Glyph ' ' Black0 Black0 (Just Black0) (Just Black0)
 ts <- mapM zConstruct [(x, y) | x <- xs, y <- ys]
 return $ mkGlyphMap $ map (\p -> (p, (emptyGlyph, False))) ts

mergeGlyphMaps :: GlyphMap -> GlyphMap -> GlyphMap
mergeGlyphMaps (MkGlyphMap gmUpdated) (MkGlyphMap gmMain) =
  MkGlyphMap (M.union gmUpdated $ M.map (\(g, _) -> (g, False)) gmMain)

glyphMapToRows :: GlyphMap -> [[(Glyph, Bool)]]
glyphMapToRows (MkGlyphMap gm) =
  let glyphList = M.toList gm
      sortedGlyphList = sortBy brickCmpPos glyphList
  in chunksOf mapWidthINT (map snd sortedGlyphList)

brickCmpPos :: (Position, a) -> (Position, a) -> Ordering
brickCmpPos (p1, _) (p2, _) =
  let (x1, y1) = unwrap p1
      (x2, y2) = unwrap p2
  in if | y1 > y2 -> GT
        | x1 > x2 -> GT
        | otherwise -> LT
