module Zwerg.Component.Position where

import Zwerg.Prelude

import Control.Exception.Base (assert)

newtype Position =
  MkPosition (Int, Int)
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Position

instance ZWrapped Position (Int, Int) where
  unwrap (MkPosition p) = p

data Metric
  = Euclidean
  | TaxiCab
  deriving (Show, Read, Eq)

{-# INLINABLE to1DIndex #-}
to1DIndex :: Position -> Int
to1DIndex pos =
  let (x, y) = unwrap pos
  in y * mapHeightINT + x

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
movePosDir :: Direction -> Position -> Position
movePosDir dir (MkPosition (x, y)) =
  MkPosition $
  if | dir == West -> (x - 1, y)
     | dir == East -> (x + 1, y)
     | dir == North -> (x, y - 1)
     | dir == South -> (x, y + 1)

{-# INLINABLE isValidPosition #-}
isValidPosition :: Position -> Bool
isValidPosition (MkPosition (x, y)) =
  x >= 0 && y >= 0 && x < mapWidthINT && y < mapHeightINT
