module Zwerg.Log (
    Log,
    HasLog(..),
    emptyLog,
    addMsg
    ) where

import Zwerg.Prelude

-- import Zwerg.Data.Color (Color)
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Control.Lens (Lens')

newtype Log = MkLog (Seq Text)
    deriving (Show, Read, Eq)

class HasLog s where
    userLog :: Lens' s Log

-- maxLogLength = 300 :: Int

emptyLog :: Log
emptyLog = MkLog S.empty

addMsg :: Text -> Log -> Log
addMsg message (MkLog l) = MkLog $ l S.|> message
