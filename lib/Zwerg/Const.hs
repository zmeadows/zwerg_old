module Zwerg.Const where

mapWidth, mapHeight :: Int
mapWidth = 15
mapHeight = 15

allChars :: String
allChars = drop 33 $ take 127 [ (minBound :: Char) .. ]
