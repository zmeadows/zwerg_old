module Zwerg.Component.AIType where

import Zwerg.Prelude

import Data.Binary
import GHC.Generics (Generic)

data AIType
  = SimpleMeleeCreature
  | SimpleRangedCreature
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary AIType
