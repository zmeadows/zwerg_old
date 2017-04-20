module Zwerg.Component.Position where

import Zwerg.Prelude

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

data Rectangle = Rectangle
  { _recW :: Int
  , _recH :: Int
  , _recX :: Int
  , _recY :: Int
  } deriving (Read, Show, Eq)

{-# INLINABLE to1DIndex #-}
to1DIndex :: Position -> Int
to1DIndex pos =
  let (x, y) = unwrap pos
  in y * mapHeightINT + x

instance ZConstructable Position (Int, Int) where
  zConstruct pos =
    if (isValidPosition pos)
      then return . MkPosition $ pos
      else throwError $
           ZError
             __FILE__
             __LINE__
             Fatal
             "Attempted to construct an invalid Position"

{-# INLINABLE distance #-}
distance
  :: Floating a
  => Metric -> Position -> Position -> a
distance metric (MkPosition (x1, y1)) (MkPosition (x2, y2)) =
  let x1' = fromIntegral x1
      y1' = fromIntegral y1
      x2' = fromIntegral x2
      y2' = fromIntegral y2
  in case metric of
       Euclidean -> sqrt $ (x1' - x2') ** 2.0 + (y1' - y2') ** 2.0
       TaxiCab -> abs (x1' - x2') + abs (y1' - y2')

{-# INLINABLE modPos #-}
modPos :: (Int -> Int, Int -> Int) -> Position -> Maybe Position
modPos (f, g) (MkPosition (x, y)) = validatePosition (f x, g y)

{-# INLINABLE movePosDir #-}
movePosDir :: Direction -> Position -> Maybe Position
movePosDir dir (MkPosition (x, y)) =
  validatePosition $
  if | dir == West -> (x - 1, y)
     | dir == East -> (x + 1, y)
     | dir == North -> (x, y - 1)
     | dir == South -> (x, y + 1)

{-# INLINABLE isValidPosition #-}
isValidPosition :: (Int, Int) -> Bool
isValidPosition (x, y) = x >= 0 && y >= 0 && x < mapWidthINT && y < mapHeightINT

{-# INLINABLE validatePosition #-}
validatePosition :: (Int, Int) -> Maybe Position
validatePosition p =
  if isValidPosition p
    then Just $ MkPosition p
    else Nothing
