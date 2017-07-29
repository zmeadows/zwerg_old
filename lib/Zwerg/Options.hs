module Zwerg.Options where

import Zwerg.Prelude

data Options = Options
  { _effectsLevel :: Int
  , _backgroundLevel :: Int
  } deriving (Show, Eq)

makeClassy ''Options

defaultOptions :: Options
defaultOptions = Options {_effectsLevel = 5, _backgroundLevel = 5}
