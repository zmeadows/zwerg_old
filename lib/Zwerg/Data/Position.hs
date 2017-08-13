module Zwerg.Data.Position
  ( Position
  , ZLevel
  , Metric(..)
  , to1DIndex
  , from1DIndex
  , unsafeFrom1DIndex
  , distance
  , modPos
  , movePosDir
  , isValidPosition
  , validatePosition
  , allPositions
  , isNeighborPos
  ) where

import Zwerg.Prelude
import Data.List (sort)

newtype ZLevel = MkZLevel Int
  deriving (Eq, Generic)
instance Binary ZLevel

newtype Position = MkPosition (Int, Int)
    deriving (Eq, Generic)

instance ZDefault Position where
    zDefault = MkPosition (0,0)


--NOTE: this Ord instance is important, as it keeps the
--GlyphMap position ordering automatic so we don't have to
--sort before drawing
instance Ord Position where
  compare (MkPosition (x1,y1)) (MkPosition (x2,y2)) =
    if (y1 /= y2)
       then compare y1 y2
       else compare x1 x2

instance Binary Position

instance ZWrapped Position (Int, Int) where
  unwrap (MkPosition p) = p
  wrap = validatePosition

data Metric = Euclidean | TaxiCab

-- data Rectangle = Rectangle
--   { _recW :: Int
--   , _recH :: Int
--   , _recX :: Int
--   , _recY :: Int
--   }

to1DIndex :: Position -> Int
to1DIndex pos =
  let (x, y) = unwrap pos
  in y * mapWidthINT + x

from1DIndex :: Int -> Maybe Position
from1DIndex i = validatePosition $ (mod i mapWidthINT, div i mapWidthINT)

unsafeFrom1DIndex :: Int -> Position
unsafeFrom1DIndex i = MkPosition (mod i mapWidthINT, div i mapWidthINT)

distance :: Metric -> Position -> Position -> Double
distance metric (MkPosition (x1, y1)) (MkPosition (x2, y2)) =
  let x1' = fromIntegral x1
      y1' = fromIntegral y1
      x2' = fromIntegral x2
      y2' = fromIntegral y2
  in case metric of
       Euclidean -> sqrt $ (x1' - x2') ** 2.0 + (y1' - y2') ** 2.0
       TaxiCab -> abs (x1' - x2') + abs (y1' - y2')

modPos :: (Int -> Int, Int -> Int) -> Position -> Maybe Position
modPos (f, g) (MkPosition (x, y)) = validatePosition (f x, g y)

movePosDir :: Direction -> Position -> Maybe Position
movePosDir dir (MkPosition (x, y)) = validatePosition $
    case dir of
        West      -> (x - 1, y)
        East      -> (x + 1, y)
        North     -> (x, y - 1)
        South     -> (x, y + 1)
        NorthWest -> (x - 1, y + 1)
        NorthEast -> (x + 1, y + 1)
        SouthWest -> (x - 1, y - 1)
        SouthEast -> (x + 1, y - 1)

isValidPosition :: (Int, Int) -> Bool
isValidPosition (x, y) = x >= 0 && y >= 0 && x < mapWidthINT && y < mapHeightINT

validatePosition :: (Int, Int) -> Maybe Position
validatePosition p =
  if isValidPosition p
    then Just $ MkPosition p
    else Nothing

allPositions :: [Position]
allPositions =
  let xs = [0 .. mapWidthINT - 1]
      ys = [0 .. mapHeightINT - 1]
  in sort $ map MkPosition [ (x,y) | x <- xs, y <- ys ]

isNeighborPos :: Position -> Position -> Bool
isNeighborPos (MkPosition (x1,y1)) (MkPosition (x2,y2)) =
    (yn && xs) || (xn && ys)
  where yn = abs (y1 - y2) == 1
        xn = abs (x1 - x2) == 1
        xs = x1 == x2
        ys = y1 == y2
