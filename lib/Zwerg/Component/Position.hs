module Zwerg.Component.Position where

import Zwerg.Const (mapWidth, mapHeight)

import Control.Exception.Base (assert)

import GHC.Generics (Generic)
import Data.Binary

newtype Position = MkPosition (Int,Int)
    deriving (Show, Read, Eq, Ord, Generic)

instance Binary Position

data Metric = Euclidean | TaxiCab
    deriving (Show, Read, Eq)

{-# INLINABLE unPosition #-}
unPosition :: Position -> (Int,Int)
unPosition (MkPosition t) = t

{-# INLINABLE to1DIndex #-}
to1DIndex :: Position -> Int
to1DIndex pos =
    let (x,y) = unPosition pos
    in x*mapWidth + y*mapHeight

{-# INLINABLE mkPosition #-}
mkPosition :: (Int,Int) -> Position
mkPosition (x,y) = assert isValidPosition $ MkPosition (x,y)
    where isValidPosition = x >= 0
                         && y >= 0
                         && x <= mapWidth
                         && y <= mapHeight

{-# INLINABLE distanceBetween #-}
distanceBetween :: Floating a => Metric -> Position -> Position -> a
distanceBetween metric (MkPosition (x1,y1)) (MkPosition (x2,y2)) =
    let x1' = fromIntegral x1
        y1' = fromIntegral y1
        x2' = fromIntegral x2
        y2' = fromIntegral y2
    in case metric of
         Euclidean -> sqrt $ (x1' - x2')**2.0 + (y1' - y2')**2.0
         TaxiCab -> abs (x1' - x2') + abs (y1' - y2')

{-# INLINABLE modifyPosition #-}
modifyPosition :: (Int -> Int, Int -> Int) -> Position -> Position
modifyPosition (f,g) (MkPosition (x,y)) = mkPosition (f x, g y)
