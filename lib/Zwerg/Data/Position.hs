module Zwerg.Data.Position
  ( Position
  , ZLevel
  , Metric(..)
  , Rectangle
  , to1DIndex
  , from1DIndex
  , distance
  , modPos
  , movePosDir
  , validatePosition
  , allPositions
  ) where

import Zwerg.Prelude

newtype ZLevel = MkZLevel Int
  deriving (Show, Eq, Ord, Generic)
instance Binary ZLevel

instance ZConstructable ZLevel Int where
  zConstruct z =
    if z >= 0
      then return $ MkZLevel z
      else $(throw) EngineFatal "Attempted to construct an invalid ZLevel."

newtype Position =
  MkPosition (Int, Int)
  deriving (Show, Eq, Generic)

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

data Metric = Euclidean | TaxiCab
  deriving (Show, Eq)

data Rectangle = Rectangle
  { _recW :: Int
  , _recH :: Int
  , _recX :: Int
  , _recY :: Int
  } deriving (Show, Eq)

{-# INLINABLE to1DIndex #-}
to1DIndex :: Position -> Int
to1DIndex pos =
  let (x, y) = unwrap pos
  in y * mapWidthINT + x

{-# INLINABLE from1DIndex #-}
from1DIndex :: Int -> Maybe Position
from1DIndex i = validatePosition $ (mod i mapWidthINT, div i mapWidthINT)

instance ZConstructable Position (Int, Int) where
  zConstruct pos =
    if isValidPosition pos
      then return . MkPosition $ pos
      else $(throw) EngineFatal "Attempted to construct an invalid Position"

{-# INLINABLE distance #-}
distance :: Metric -> Position -> Position -> Double
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

allPositions :: [Position]
allPositions =
  let xs = [0 .. mapWidthINT - 1]
      ys = [0 .. mapHeightINT - 1]
  in map MkPosition [ (x,y) | x <- xs, y <- ys ]
