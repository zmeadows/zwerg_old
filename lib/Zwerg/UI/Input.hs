module Zwerg.UI.Input where

data Input = CharInput Char | Escape | Tab
    deriving (Show, Read, Eq, Ord)
