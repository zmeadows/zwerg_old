module Zwerg.Data.GridMap (GridMap) where

import Zwerg.Prelude
import Zwerg.Data.Position

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M (traverseWithKey, lookup, fromList, adjust, empty)
import Data.Maybe (fromJust)

newtype GridMap a = MkGridMap (IntMap a)
    deriving (Eq, Show, Generic)
instance Binary a => Binary (GridMap a)

instance ZDefault (GridMap a) where
    zDefault = MkGridMap M.empty

instance ZCompleteMapContainer (GridMap a) Position a where
    zAt (MkGridMap m) pos = fromJust $ M.lookup (to1DIndex pos) m
    zAdjust f k (MkGridMap m) = MkGridMap $ M.adjust f (to1DIndex k) m
    zBuild f = MkGridMap $ M.fromList $ map (\p -> (to1DIndex p,) $ f p) allPositions
    zBuildM f = MkGridMap . M.fromList <$> mapM (\p -> (to1DIndex p,) <$> f p) allPositions

instance ZTraversable2 GridMap Position where
    zTraverseWithKey (MkGridMap gm) f = MkGridMap <$> M.traverseWithKey (f . fromJust . from1DIndex) gm

-- mergeUpdates :: GridMap a -> [(Position, a)] -> GridMap a
-- mergeUpdates (MkGridMap m) ps = MkGridMap $ M.union (M.fromList $ map convert ps) m
--     where convert (p,x) = (to1DIndex p,x)

