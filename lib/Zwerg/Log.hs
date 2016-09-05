module Zwerg.Log (
    Message,
    Log,
    HasLog(..),
    emptyLog,
    addMsg
    ) where

import Zwerg.Data.Color (Color)
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Control.Lens (Lens')

type Message = [(Color,Text)]

newtype Log = MkLog (Seq Message)
    deriving (Show, Read, Eq)

class HasLog s where
    userLog :: Lens' s Log

--maxLogLength = 300 :: Int

emptyLog :: Log
emptyLog = MkLog S.empty

addMsg :: Message -> Log -> Log
addMsg msg (MkLog l) = MkLog $ l S.|> msg
