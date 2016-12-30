module Zwerg.UI.Font where

import GHC.Generics (Generic)
import Data.Binary
import Data.Hashable

data FontType = Normal | Bold | Italic | BoldItalic
    deriving (Show, Read, Eq, Ord, Enum, Generic, Bounded)

instance Binary FontType
instance Hashable FontType
