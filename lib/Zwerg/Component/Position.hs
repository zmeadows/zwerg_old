module Zwerg.Component.Position where

import Zwerg.Const (mapWidth, mapHeight)
import qualified Zwerg.Data.Direction as Direction
import Zwerg.Prelude

import Control.Exception.Base (assert)

import Data.Binary
import GHC.Generics (Generic)

newtype Position =
  MkPosition (Int, Int)
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Position

data Metric
  = Euclidean
  | TaxiCab
  deriving (Show, Read, Eq)

{-# INLINABLE unPosition #-}
unPosition :: Position -> (Int, Int)
unPosition (MkPosition t) = t

{-# INLINABLE to1DIndex #-}
to1DIndex :: Position -> Int
to1DIndex pos =
  let (x, y) = unPosition pos
  in y * round mapHeight + x

{-# INLINABLE mkPosition #-}
mkPosition :: (Int, Int) -> Position
mkPosition (x, y) =
  let pos = MkPosition (x, y)
  in assert (isValidPosition pos) pos

{-# INLINABLE distanceBetween #-}
distanceBetween
  :: Floating a
  => Metric -> Position -> Position -> a
distanceBetween metric (MkPosition (x1, y1)) (MkPosition (x2, y2)) =
  let x1' = fromIntegral x1
      y1' = fromIntegral y1
      x2' = fromIntegral x2
      y2' = fromIntegral y2
  in case metric of
       Euclidean -> sqrt $ (x1' - x2') ** 2.0 + (y1' - y2') ** 2.0
       TaxiCab -> abs (x1' - x2') + abs (y1' - y2')

{-# INLINABLE modifyPosition #-}
modifyPosition :: (Int -> Int, Int -> Int) -> Position -> Position
modifyPosition (f, g) (MkPosition (x, y)) = mkPosition (f x, g y)

{-# INLINABLE movePosDir #-}
movePosDir :: Direction.Direction -> Position -> Position
movePosDir dir (MkPosition (x, y)) =
  MkPosition $
  if | dir == Direction.Left -> (x - 1, y)
     | dir == Direction.Right -> (x + 1, y)
     | dir == Direction.Up -> (x, y - 1)
     | dir == Direction.Down -> (x, y + 1)

{-# INLINABLE isValidPosition #-}
isValidPosition :: Position -> Bool
isValidPosition (MkPosition (x, y)) =
  x >= 0 && y >= 0 && x < round mapWidth && y < round mapHeight
