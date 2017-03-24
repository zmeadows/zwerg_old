module Zwerg.Data.Error where

import Zwerg.Prelude

import Data.Text (Text)
import Control.Lens (makeClassy)

data ZErrorLevel = Warning | Fatal
  deriving (Show, Read, Eq)

data ZError = ZError
  { _file        :: Text
  , _line        :: Int
  , _errLevel    :: ZErrorLevel
  , _description :: Text
  } deriving (Show, Read, Eq)
makeClassy ''ZError
