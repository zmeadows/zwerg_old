module Zwerg.UI.Input where

import Zwerg.Prelude

data KeyCode =
      KeyChar Char
    | Return
    | Escape
    | Backspace
    | Tab
    | UpArrow
    | DownArrow
    | LeftArrow
    | RightArrow
    deriving (Show, Read, Eq)
