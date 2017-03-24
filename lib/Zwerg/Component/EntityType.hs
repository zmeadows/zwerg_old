module Zwerg.Component.EntityType where

import Zwerg.Prelude

import GHC.Generics (Generic)
import Data.Binary

data EntityType = Player | Enemy | Item | Tile | Level | Container
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary EntityType
