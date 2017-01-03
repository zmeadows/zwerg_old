module Zwerg.UI.Port where

import Zwerg.UI.Menu

import Data.Text (Text)

import Control.Lens (makeClassy)

data Port =
      Inventory
    | Map
    | Main
    | ChooseTarget
    | MainMenu (Menu Text)
    deriving (Show, Eq)
makeClassy ''Port

initMainMenu :: Port
initMainMenu = MainMenu $ makeMenu [ "new game", "load game", "options", "about", "exit" ]
