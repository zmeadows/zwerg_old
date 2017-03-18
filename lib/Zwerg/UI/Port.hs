module Zwerg.UI.Port where

import Zwerg.UI.Menu

import Control.Lens (makeClassy)

data Port =
      Inventory
    | Map
    | MainScreen
    | ChooseTarget
    | MainMenu TextMenu
    | ExitScreen
    deriving (Show, Eq)
makeClassy ''Port

initMainMenu :: Port
initMainMenu = MainMenu $ makeTextMenu [ "new game", "load game", "options", "about", "exit" ]
