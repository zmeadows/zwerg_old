module Zwerg.Component.TileType where

data TileType = Floor | Wall | Door | Void
    deriving (Show, Read, Eq)
