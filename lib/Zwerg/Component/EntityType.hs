module Zwerg.Component.EntityType where

data EntityType = Player | Enemy | Item | Tile
    deriving (Show, Read, Eq, Ord, Enum)
