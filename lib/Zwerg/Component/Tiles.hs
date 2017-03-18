module Zwerg.Component.Tiles (
    Tiles,
    mkTiles,
    getTileUUIDAtPos,
    filterTilesM
    ) where

import Zwerg.Component.Position
import Zwerg.Component.UUID

import Data.Vector (Vector)
import Data.Vector.Binary()
import qualified Data.Vector as V

import Control.Exception.Base (assert)

import GHC.Generics (Generic)
import Data.Binary

import qualified Data.List as L (sortBy, nub)

newtype Tiles = MkTiles (Vector UUID)
    deriving (Show, Read, Eq, Generic)

instance Binary Tiles

mkTiles :: [(UUID, Position)] -> Tiles
mkTiles upl = assert isValidTileList
                $ MkTiles $ V.fromList $ map fst upl'
    where upl' = L.sortBy (\(_,p1) (_,p2) -> positionSorter p1 p2) upl
          isValidTileList = let ps = map snd upl
                            in length (L.nub ps) == length ps

positionSorter :: Position -> Position -> Ordering
positionSorter p1 p2
    | pi1 > pi2 = GT
    | pi1 < pi2 = LT
    | otherwise = EQ
  where pi1 = to1DIndex p1
        pi2 = to1DIndex p2

getTileUUIDAtPos :: Position -> Tiles -> UUID
getTileUUIDAtPos p (MkTiles v) = v V.! to1DIndex p

filterTilesM :: Monad m => (UUID -> m Bool) -> Tiles -> m [UUID]
filterTilesM f (MkTiles t) = V.toList <$> V.filterM f t


