module Zwerg.Util where

import Zwerg.Prelude

import Control.Exception.Base (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Paths_zwerg (getDataFileName)

import Unsafe

getAsset
  :: MonadIO m
  => Text -> m Text
getAsset path = pack <$> liftIO (getDataFileName $ "assets/" ++ unpack path)

assertfM
  :: Monad m
  => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

whenJust
  :: Applicative m
  => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenJustErr
  :: (MonadError ZError m)
  => Maybe a -> ZError -> (a -> m b) -> m b
whenJustErr mg err f = maybe (throwError err) f mg

-- | Like 'whenJust', but where the test can be monadic.
whenJustM
  :: Monad m
  => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

fromJustErrM
  :: (MonadError ZError m)
  => Maybe a -> ZError -> m a
fromJustErrM x err = maybe (throwError err) return x

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    build g = g (:) []
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

-- | See <http://roguebasin.roguelikedevelopment.org/index.php/Digital_lines>.
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps
  | eps + p < q = 0 : balancedWord p q (eps + p)
  | otherwise = 1 : balancedWord p q (eps + p - q)

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

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line p1 p2 = p2 : takeWhile (/= p2) (bla p1 p2)
