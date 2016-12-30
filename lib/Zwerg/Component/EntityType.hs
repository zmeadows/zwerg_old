module Zwerg.Component.EntityType where

import GHC.Generics (Generic)
import Data.Binary

data EntityType = Player | Enemy | Item | Tile
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary EntityType
