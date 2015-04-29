-- {-# LANGUAGE CPP, TemplateHaskell, TypeOperators, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Zwerg.Layer.Simple where

import Zwerg.Entity
import Zwerg.Types

import qualified Data.IntMap.Strict as I

import Data.Label.Monadic

makeSimpleLayer :: Int -> Int -> System Int
makeSimpleLayer w h = do
    layerUUID <- getNextUUID
    name =. I.insert layerUUID "Simple Layer"
    entityType =. I.insert layerUUID Layer
    width =. I.insert layerUUID w
    height =. I.insert layerUUID h
    depth =. I.insert layerUUID 1
    tiles =. I.insert layerUUID []
    nextUUID  =. (+ 1)
    let makeSimpleTile (x,y)
            | x == 0 || y == 0 || x == w - 1 || y == h - 1 = makeTile Wall layerUUID (x,y)
            | otherwise = makeTile Floor layerUUID (x,y)
    mapM_ makeSimpleTile squares
    return layerUUID
  where squares = [(i,j) | i <- [0..w-1], j <- [0..h-1]]

makeTile :: TileType -> Int -> Position -> System ()
makeTile tt layerUUID pos = do
    uuid       <- getNextUUID
    entityType  =. I.insert uuid Tile
    position    =. I.insert uuid pos
    tileType    =. I.insert uuid tt
    glyph       =. I.insert uuid (tileTypeToChar tt, Attributes FontRegular White Black 0 0)
    occupants   =. I.insert uuid []
    layer       =. I.insert uuid layerUUID
    needsRedraw =. I.insert uuid True
    blocked     =. I.insert uuid (tt /= Floor)
    tiles       =. I.adjust (uuid :) layerUUID

tileTypeToChar :: TileType -> Char
tileTypeToChar Door = '%'
tileTypeToChar Floor = '.'
tileTypeToChar Wall = '#'
tileTypeToChar Void = ' '

