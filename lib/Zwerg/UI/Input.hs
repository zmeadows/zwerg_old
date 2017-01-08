module Zwerg.UI.Input where

data KeyCode =
    KeyChar Char
    | Return
    | Escape
    | Backspace
    | Tab
    deriving (Show, Read, Eq)
