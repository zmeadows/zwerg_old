module Zwerg.Log
  ( Log
  , HasLog(..)
  , emptyLog
  , pushLogMsg
  ) where

import Zwerg.Class
import Zwerg.Prelude

import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)

import Control.Lens (Lens', (%=))

newtype Log =
  MkLog (Seq Text)
  deriving (Show, Read, Eq)

class HasLog s where
  userLog :: Lens' s Log

instance ZWrapped Log (Seq Text) where
  unwrap (MkLog l) = l

emptyLog :: Log
emptyLog = MkLog S.empty

pushLogMsg
  :: (HasLog s, MonadState s m)
  => Text -> m ()
pushLogMsg message = userLog %= \l -> MkLog $ (unwrap l) S.|> message
