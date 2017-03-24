module Zwerg.Data.Direction where

import Zwerg.Prelude

import GHC.Generics (Generic)
import Data.Binary

data Direction = Up | Down | Left | Right
    deriving (Show, Read, Eq, Generic)

instance Binary Direction
