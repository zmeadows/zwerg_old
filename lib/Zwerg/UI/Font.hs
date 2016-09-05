module Zwerg.UI.Font where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data FontType = Normal | Bold | Italic | BoldItalic
    deriving (Show, Read, Eq, Ord, Enum, Generic, Bounded)

instance Hashable FontType
