module Zwerg.Data.GridMap
  ( GridMap
  , atPos
  , traverseWithPos
  , traverseWithPos_
  , makeGridMapM
  , makeGridMap
  , gridMapElems
  ) where

import Zwerg.Prelude
import Zwerg.Data.Position

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M (traverseWithKey, lookup, fromList, size, elems)
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

makeGridMapM :: Monad m => (Position -> m a) -> m (GridMap a)
makeGridMapM f = MkGridMap . M.fromList <$> mapM (\p -> (to1DIndex p,) <$> f p) allPositions

makeGridMap :: (Position -> a) -> GridMap a
makeGridMap f = MkGridMap $ M.fromList $ map (\p -> (to1DIndex p,) $ f p) allPositions

atPos :: Position -> GridMap a -> a
atPos pos (MkGridMap m) = fromJust $ M.lookup (to1DIndex pos) m

traverseWithPos :: Applicative t => GridMap a -> (Position -> a -> t b) -> t (GridMap b)
traverseWithPos (MkGridMap gm) f = MkGridMap <$> M.traverseWithKey (\i -> f $ (fromJust $ from1DIndex i)) gm

traverseWithPos_ :: Applicative t => GridMap a -> (Position -> a -> t b) -> t ()
traverseWithPos_ g f = void $ traverseWithPos g f

gridMapElems :: GridMap a -> [a]
gridMapElems (MkGridMap gm) = M.elems gm
