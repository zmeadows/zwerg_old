module Zwerg.Data.GridMap (GridMap, mergeUpdates) where

import Zwerg.Prelude
import Zwerg.Data.Position

import Data.Vector (Vector, (//))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Vector.Binary()

newtype GridMap a = MkGridMap (Vector a)
    deriving stock (Functor, Generic)
    deriving anyclass Binary

instance ZDefault a => ZDefault (GridMap a) where
    zDefault = MkGridMap $ V.replicate (mapWidthINT * mapHeightINT) zDefault

instance ZDefault a => ZWrapped (GridMap a) (Vector a) where
    unwrap (MkGridMap v) = v
    wrap _ = Just (zDefault :: GridMap a)

instance ZMapContainer GridMap Position where
    zModifyAt f k (MkGridMap m) = MkGridMap $ V.modify (\mv -> VM.modify mv f (to1DIndex k)) m
    zElems (MkGridMap m) = V.toList m

instance ZCompleteMapContainer GridMap Position where
    zAt (MkGridMap m) pos = m V.! (to1DIndex pos)
    zIndices = allPositions
    zBuild f = MkGridMap $ V.fromList $ map f allPositions
    zBuildM f = MkGridMap . V.fromList <$> mapM f allPositions

instance ZTraversable2 GridMap Position where
    zTraverseWithKey (MkGridMap gm) f =
        MkGridMap <$> V.imapM (\p x -> f (unsafeFrom1DIndex p) x) gm

mergeUpdates :: GridMap a -> [(Position, a)] -> GridMap a
mergeUpdates (MkGridMap m) ps = MkGridMap $ m // (map convert ps)
    where convert (p,x) = (to1DIndex p,x)

