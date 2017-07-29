module Zwerg.UI.Port where

import Zwerg.Prelude
import Zwerg.Component.Position
import Zwerg.UI.GlyphMap
import Zwerg.UI.Menu

data Port
  = MainScreen GlyphMap
  | MainMenu (Menu ())
  | ChooseTarget
  | LoadingScreen
  | ViewEquipment
  | ViewInventory (Menu InventoryMenuItem)
  | PickupItems (Menu UUID)
  | ExamineTiles Position
  | DeathScreen
  | ExitScreen
  deriving (Show, Eq, Generic)

data InventoryMenuItem = InventoryMenuItem
  { _itemUUID        :: UUID
  , _longDescription :: Text
  } deriving (Show, Eq, Generic)
makeClassy ''InventoryMenuItem
instance Binary InventoryMenuItem

instance Binary Port

type Portal = [Port]

class HasPortal s where
  portal :: Lens' s Portal

initMainMenu :: Port
initMainMenu =
  MainMenu $ makeMenu $
  zip ["new game", "load game", "options", "about", "exit"] $ repeat ()
