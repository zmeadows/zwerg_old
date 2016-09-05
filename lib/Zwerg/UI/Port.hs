module Zwerg.UI.Port where

data Port =
      Inventory
    | Map
    | Main
    | ChooseTarget
    | MainMenu
    deriving (Show, Read, Eq)


