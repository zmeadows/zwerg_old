module Zwerg.Data.GridMap
  ( GridMap
  , atPos
  , traverseWithPos
  , traverseWithPos_
  ) where

import Zwerg.Prelude
import Zwerg.Data.Position

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M (traverseWithKey, lookup, fromList, size)
import Data.Maybe (fromJust)

newtype GridMap a = MkGridMap (IntMap a)
    deriving (Eq, Show, Generic)
instance Binary a => Binary (GridMap a)

instance ZConstructable (GridMap a) [(Position,a)] where
  zConstruct l =
    let l' = map (\(pos,x) -> (to1DIndex pos, x)) l
        m = M.fromList l'
     in if (M.size m == mapHeightINT * mapWidthINT)
        then return $ MkGridMap m
        else $(throw) EngineFatal
             "Attempted to construct a TileMap with number of tiles not equal to mapWidth * mapHeight"

atPos :: Position -> GridMap a -> a
atPos pos (MkGridMap m) = fromJust $ M.lookup (to1DIndex pos) m

traverseWithPos :: Applicative t => GridMap a -> (Position -> a -> t b) -> t (GridMap b)
traverseWithPos (MkGridMap gm) f = MkGridMap <$> M.traverseWithKey (\i -> f $ (fromJust $ from1DIndex i)) gm

traverseWithPos_ :: Applicative t => GridMap a -> (Position -> a -> t b) -> t ()
traverseWithPos_ g f = void $ traverseWithPos  g f
