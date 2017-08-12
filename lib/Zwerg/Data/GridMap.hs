module Zwerg.Data.GridMap (GridMap, mergeUpdates) where

import Zwerg.Prelude
import Zwerg.Data.Position

import Data.Maybe (fromJust)
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
  ( traverseWithKey
  , lookup
  , fromList
  , adjust
  , empty
  , union
  , elems
  , map
  )

newtype GridMap a = MkGridMap (IntMap a)
    deriving (Eq, Show, Generic)
instance Binary a => Binary (GridMap a)

instance ZDefault (GridMap a) where
    zDefault = MkGridMap M.empty

instance ZMapContainer GridMap Position where
    zModifyAt f k (MkGridMap m) = MkGridMap $ M.adjust f (to1DIndex k) m
    zElems (MkGridMap m) = M.elems m

instance ZCompleteMapContainer GridMap Position where
    zAt (MkGridMap m) pos = fromJust $ M.lookup (to1DIndex pos) m
    zIndices = allPositions
    zBuild f = MkGridMap $ M.fromList $ map (\p -> (to1DIndex p,) $ f p) allPositions
    zBuildM f = MkGridMap . M.fromList <$> mapM (\p -> (to1DIndex p,) <$> f p) allPositions

instance ZTraversable2 GridMap Position where
    zTraverseWithKey (MkGridMap gm) f =
        MkGridMap <$> M.traverseWithKey (f . fromJust . from1DIndex) gm

instance ZFoldable GridMap where
    zMap f (MkGridMap m) = MkGridMap $ M.map f m

-- TODO: use list update function instead of union from IntMap
mergeUpdates :: GridMap a -> [(Position, a)] -> GridMap a
mergeUpdates (MkGridMap m) ps = MkGridMap $ M.union (M.fromList $ map convert ps) m
    where convert (p,x) = (to1DIndex p,x)

