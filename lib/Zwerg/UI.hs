module Zwerg.UI where

import Zwerg.UI.Port

import Control.Lens (makeClassy)

data UserInterface = UserInterface
    { _port        :: Port
    } deriving (Show, Eq)
makeClassy ''UserInterface

initUI :: UserInterface
initUI = UserInterface
    { _port = MainMenu initMainMenu
    }

