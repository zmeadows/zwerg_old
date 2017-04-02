module Zwerg.UI.Font where

import Zwerg.Prelude

import Data.Binary
import Data.Hashable
import GHC.Generics (Generic)

data FontType
  = Normal
  | Standout
  | Underline
  | Dim
  | Blink
  | Bold
  deriving (Show, Read, Eq, Ord, Enum, Generic, Bounded)

instance Binary FontType

instance Hashable FontType
