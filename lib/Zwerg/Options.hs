module Zwerg.Options where

import Zwerg.Prelude

import Control.Lens (makeClassy)

data Options = Options
    { _effectsLevel    :: Int
    , _backgroundLevel :: Int
    } deriving (Show, Read, Eq)
makeClassy ''Options

defaultOptions :: Options
defaultOptions = Options
    { _effectsLevel = 5
    , _backgroundLevel = 5
    }
