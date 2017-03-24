module Zwerg.UI.Port where

import Zwerg.Prelude
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
initMainMenu = MainMenu $ makeMenu $ zip ["new game", "load game", "options", "about", "exit" ] $ repeat ()
