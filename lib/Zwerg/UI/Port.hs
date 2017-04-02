module Zwerg.UI.Port where

import Zwerg.Prelude
import Zwerg.UI.GlyphMap
import Zwerg.UI.Menu

data Port
  = MainScreen GlyphMap
  | Inventory
  | ChooseTarget
  | LoadingScreen
  | MainMenu TextMenu
  | ExitScreen
  deriving (Show, Eq)

type Portal = [Port]

class HasPortal s where
  portal :: Lens' s Portal

initMainMenu :: Port
initMainMenu =
  MainMenu $
  makeMenu $
  zip ["new game", "load game", "options", "about", "exit"] $ repeat ()
