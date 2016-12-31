module Zwerg.UI.Input where

data KeyCode = KeyChar Char | Return | Escape deriving (Show, Read, Eq)

data KeyMod = Shift | Control | None
  deriving (Show, Read, Eq)

type Key = (KeyMod, KeyCode)
