module Zwerg.UI where

import Zwerg.UI.Port (Port(..))
import Zwerg.Options (Options, defaultOptions)

import Control.Lens (makeClassy)
import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Z

data UserInterface = UserInterface
    { _port        :: Port
    , _options     :: Options
    } deriving (Show, Read, Eq)
makeClassy ''UserInterface

initUI :: UserInterface
initUI = UserInterface
    { _port = MainMenu
    , _options = defaultOptions
    }

type Menu a = Zipper a
