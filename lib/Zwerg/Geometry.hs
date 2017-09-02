module Zwerg.Geometry
  ( line
  , circle
  , squareSpiral
  ) where

import Zwerg.Prelude

-------------------------------------------------------------------------------

{-# INLINABLE circle #-}
circle :: (Int, Int) -> Int -> [(Int, Int)]
circle p0 r = circle' p0 (r, 0) 0 []

circle' :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
circle' (x0, y0) (x, y) err s =
  let err' = if err <= 0 then err + 2 * y + 1 else err - (2 * x + 1)
      x' = if err <= 0 then x else x - 1
      y' = y + 1
      --go (p1, p2) ps = if p1 >= 0 && p2 >= 0 then (p1, p2) : ps else ps
      go (p1, p2) ps = (p1, p2) : ps
      s' = go (x0 + x, y0 + y) $
           go (x0 + y, y0 + x) $
           go (x0 - x, y0 + y) $
           go (x0 - y, y0 + x) $
           go (x0 + x, y0 - y) $
           go (x0 + y, y0 - x) $
           go (x0 - x, y0 - y) $
           go (x0 - y, y0 - x) s
  in if | x >= y -> circle' (x0, y0) (x', y') err' s'
        | otherwise -> s'

-------------------------------------------------------------------------------

{-# INLINABLE line #-}
line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line = bla

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx, y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step)
        | abs dx > abs dy = (abs dy, abs dx, xyStep)
        | otherwise = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in walk (balancedWord p q 0) (x0, y0)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps
  | eps + p < q = 0 : balancedWord p q (eps + p)
  | otherwise = 1 : balancedWord p q (eps + p - q)

-------------------------------------------------------------------------------

squareSpiral :: Int -> [(Int,Int)]
squareSpiral radius = go 1 radius [(radius,0)]
    where go :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
          go 2 1 sps@((1,1):_) = sps
          go 1 r sps@((x,1):_) = go 1 (r-1) ((x-1,0):sps)
          go 0 r sps@((x,y):_) = if y == r then go 2 r ((x+1,y):sps) else go 0 r ((x,y+1):sps)
          go 1 r sps@((x,y):_) = if y == -r then go 3 r ((x-1,y):sps) else go 1 r ((x,y-1):sps)
          go 2 r sps@((x,y):_) = if x == r then go 1 r ((x,y+1):sps) else go 2 r ((x+1,y):sps)
          go 3 r sps@((x,y):_) = if x == -r then go 0 r ((x,y-1):sps) else go 3 r ((x-1,y):sps)
          go _ _ _ = error "apparently impossible case encountered while building square spiral"

-- diamondSpiral :: Int -> [(Int,Int)]
-- diamondSpiral _ = []
