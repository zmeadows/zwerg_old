module Zwerg.Data.Target where

import Zwerg.Prelude

import Data.Binary
import GHC.Generics (Generic)
import Zwerg.Data.Direction

data TargetType
  = SingleTarget
  | AOE Double
  | Line Direction
         Int
  deriving (Show, Read, Eq, Generic)

instance Binary TargetType
