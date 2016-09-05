module Zwerg.Component.Glyph where

import Zwerg.Data.Color
import Zwerg.UI.Font

data Glyph = Glyph
    { fontType :: FontType
    , char     :: Char
    , color    :: Color
    } deriving (Show, Read, Eq)

