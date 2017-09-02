module Zwerg.UI.Port where

import Zwerg.Prelude

import Zwerg.Data.Position
import Zwerg.UI.Menu

import qualified Data.List.NonEmpty as NE (repeat, zip)

data Port
    = MainScreen
    | MainMenu (Menu ())
    | ChooseTarget
    | LoadingScreen
    | ViewEquipment
    | ViewInventory (MenuGroupSelect UUID)
    | ExamineTiles Position
    | DeathScreen Text
    | ExitScreen
  deriving stock Generic
  deriving anyclass Binary

instance ZDefault Port where
    zDefault = initMainMenu

type Portal = NonEmpty Port

initMainMenu :: Port
initMainMenu =
  MainMenu $ makeMenu $
  NE.zip ("new game" :| ["load game", "options", "about", "exit"]) $ NE.repeat ()
