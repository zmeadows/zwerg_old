module Zwerg.UI.Port where

import Zwerg.Prelude

import Zwerg.Data.Position
import Zwerg.UI.GlyphMap
import Zwerg.UI.Menu

import qualified Data.List.NonEmpty as NE (repeat, zip)

data Port
  = MainScreen GlyphMap
  | MainMenu (Menu ())
  | ChooseTarget
  | LoadingScreen
  | ViewEquipment
  | ViewInventory (MenuGroupSelect UUID)
  | ExamineTiles Position
  | DeathScreen Text
  | ExitScreen
  deriving (Generic)
instance Binary Port

instance ZDefault Port where
    zDefault = initMainMenu

type Portal = [Port]

class HasPortal s where
  portal :: Lens' s Portal

initMainMenu :: Port
initMainMenu =
  MainMenu $ makeMenu $
  NE.zip ("new game" :| ["load game", "options", "about", "exit"]) $ NE.repeat ()
