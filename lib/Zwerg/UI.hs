module Zwerg.UI where

import Zwerg.UI.Port (Port(..))
import Zwerg.Options (Options, defaultOptions)

import Control.Lens (makeClassy)

data UserInterface = UserInterface
    { _port        :: Port
    , _options     :: Options
    } deriving (Show, Read, Eq)
makeClassy ''UserInterface

initUI :: UserInterface
initUI = UserInterface
    { _port = Inventory
    , _options = defaultOptions
    }
