module Zwerg.Types where

import Data.Text(Text)

data Direction = North | South | East | West deriving (Show, Read, Eq)

data Color = Default | White | Black | Green | Red | Blue deriving (Show, Read, Eq)

data KeyCode = Letter Char | SemiColon | Escape deriving (Eq, Show)

data KeyMod = Shift | Control | None deriving (Show, Eq)

data TileType = Wall | Door | Floor | Void deriving (Show, Read, Eq)

type Position = (Int,Int)

type Glyph = (Char, Color, Color)

data ZWERGError = PlayerError Text
                | FatalError Integer Text deriving (Show, Eq)

