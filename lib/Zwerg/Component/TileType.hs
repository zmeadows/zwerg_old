module Zwerg.Component.TileType where

import GHC.Generics (Generic)
import Data.Binary

data TileType = Floor | Wall | Door | Void
    deriving (Show, Read, Eq, Generic)

instance Binary TileType
