{-# LANGUAGE TemplateHaskell #-}

module Zwerg.Types where

import Data.Label (fclabels)
import Data.Text (Text)

data Direction = North | South | East | West deriving (Show, Read, Eq)

data Color =  White | Black | Red | Green | Blue deriving (Show, Read, Eq, Ord)

data KeyCode = Letter Char | Return | Escape deriving (Eq, Show)

data KeyMod = Shift | Control | None deriving (Show, Eq)

data TileType = Wall | Door | Floor | Void deriving (Show, Read, Eq)

type Position = (Int,Int)

data FontType = FontRegular | FontBold | FontItalic | FontBoldItalic deriving (Show, Read, Eq, Ord)

fclabels [d|
    data Attributes = Attributes {
        fontType  :: FontType,
        foreground :: Color,
        background :: Color,
        foregroundAlpha :: Double,
        backgroundAlpha :: Double
    } deriving (Show, Read, Eq, Ord)
  |]

type Glyph = (Char, Attributes)
type ZString = (Text, Attributes)

data ZWERGError = PlayerError Text
                | FatalError Integer Text deriving (Show, Eq)

_DEFAULT_ATTR :: Attributes
_DEFAULT_ATTR = Attributes FontRegular White Black 0 0

_PLAYER_ERR_ATTR :: Attributes
_PLAYER_ERR_ATTR = Attributes FontBold Red Black 0 0
