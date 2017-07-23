module Zwerg.Util.Geometry
  ( line
  , circle
  ) where

import Zwerg.Prelude

import Unsafe

circle :: (Int, Int) -> Int -> [(Int, Int)]
circle p0 r = circle' p0 (r, 0) 0 []

circle' :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)] -> [(Int, Int)]
circle' (x0, y0) (x, y) err s =
  let err' = if err <= 0 then err + 2 * y + 1 else err - (2 * x + 1)
      x' = if err <= 0 then x else x - 1
      y' = y + 1
      go (p1, p2) ps = if p1 >= 0 && p2 >= 0 then (p1, p2) : ps else ps
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

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line p1 p2 = takeWhile (/= p2) (bla p1 p2) ++ [p2]

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
      walk w xy = xy : walk (unsafeTail w) (step (unsafeHead w) xy)
  in walk (balancedWord p q 0) (x0, y0)

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps
  | eps + p < q = 0 : balancedWord p q (eps + p)
  | otherwise = 1 : balancedWord p q (eps + p - q)

-- data FOVContext = FOVContext
--   { _fovRange :: Double
--   , _tileMap :: Map (Int, Int) (Bool, UUID)
--   , _playerPos :: (Int, Int)
--   }
-- 
-- type FOVAlgorithm = StateT UUIDSet (Reader FOVContext) ()
-- 
-- makeTileMap :: MonadCompReader (Map (Int, Int) (Bool, UUID))
-- makeTileMap = return M.empty
-- 
-- runFOValg :: FOVAlgorithm -> MonadCompReader UUIDSet
-- runFOValg _ = return zEmpty
-- 
-- -- FOV --
-- simpleFOV :: FOVAlgorithm
-- simpleFOV = return ()
