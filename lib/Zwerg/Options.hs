module Zwerg.Options where

import Zwerg.Prelude

data Options = Options
  { effectsLevel :: Int
  , backgroundLevel :: Int
  }

defaultOptions :: Options
defaultOptions = Options
    { effectsLevel = 5
    , backgroundLevel = 5
    }
