module Zwerg.Component.Glyph where

import Zwerg.Data.Color
import Zwerg.UI.Font

import GHC.Generics (Generic)
import Data.Binary

data Glyph = Glyph
    { fontType :: FontType
    , char     :: Char
    , color    :: Color
    } deriving (Show, Read, Eq, Generic)

instance Binary Glyph
