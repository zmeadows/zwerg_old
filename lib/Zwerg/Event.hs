module Zwerg.Event (Event(..)) where

import Zwerg.Action (Action)
import Zwerg.UI.Animation (Animation)
import Zwerg.Sound (Sound)

import Control.Lens (makeClassy)

data Event = Event
    { _action    :: Action
    , _animation :: Maybe Animation
    , _sound     :: Maybe Sound
    } deriving (Show, Read, Eq)
makeClassy ''Event
