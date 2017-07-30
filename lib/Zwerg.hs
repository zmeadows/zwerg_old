module Zwerg
  ( ZwergState
  , HasZwergState(..)
  , initZwergState
  ) where

import Zwerg.Prelude hiding ((<>))

import Zwerg.Component
import Zwerg.Game
import Zwerg.Log
import Zwerg.Random

import Data.ByteString.Lazy (ByteString)

data ZwergState = ZwergState
  { _zsGameState :: GameState
  , _ranGen      :: RanGen
  , _quitting    :: Bool
  , _pastState   :: [ByteString]
  , _errorMsg    :: Maybe Text
  }
makeClassy ''ZwergState

instance HasGameState ZwergState where
  gameState = zsGameState
instance HasComponents ZwergState where
  components = gameState . components
instance HasLog ZwergState where
  userLog = gameState . userLog

initZwergState :: ZwergState
initZwergState = ZwergState
  { _zsGameState = emptyGameState
  , _ranGen      = pureRanGen 0
  , _quitting    = False
  , _pastState   = []
  , _errorMsg    = Nothing
  }


