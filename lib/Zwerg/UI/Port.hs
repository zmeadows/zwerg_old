module Zwerg.UI.Port where

import Zwerg.UI.Menu

import Data.Text

data Port =
      Inventory
    | Map
    | Main
    | ChooseTarget
    | MainMenu (Menu Text)
    deriving (Show, Eq)

initMainMenu :: Menu Text
initMainMenu = makeMenu [ "new game", "load game", "options", "about", "exit" ]
