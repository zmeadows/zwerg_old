module Zwerg.UI.Input where

import Zwerg.Prelude

data KeyCode =
    KeyChar Char
    | Return
    | Escape
    | Backspace
    | Tab
    deriving (Show, Read, Eq)
