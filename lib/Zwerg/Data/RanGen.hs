module Zwerg.Data.RanGen (
  RanGen,
  RanChoice(..),
  newPureRanGen,
  pureRanGen
  ) where

import Zwerg.Component.UUID
import Zwerg.Data.UUIDSet
import Zwerg.Class
import Zwerg.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Random       
import Data.Word (Word64)
import System.Random.Mersenne.Pure64 (PureMT, pureMT, newPureMT)
import qualified Data.Set as S

type RanGen = PureMT

pureRanGen :: Word64 -> RanGen
pureRanGen = pureMT

newPureRanGen :: (MonadIO m) => m RanGen
newPureRanGen = liftIO newPureMT

class RanChoice a b | a -> b where
  pickRandom :: (MonadRandom m) => a -> m b

instance RanChoice UUIDSet UUID where
  {-# INLINABLE pickRandom #-}
  pickRandom us =
    let uws = unwrap us
        maxInd = (S.size uws) - 1
    in flip S.elemAt uws <$> getRandomR (0, maxInd)
  
