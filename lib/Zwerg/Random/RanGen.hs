module Zwerg.Random.RanGen
  ( RanGen
  , RanChoice(..)
  , newPureRanGen
  , pureRanGen
  ) where

import Unsafe (unsafeIndex)
import Zwerg.Data.UUIDSet
import Zwerg.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word64)
import System.Random.Mersenne.Pure64 (PureMT, pureMT, newPureMT)

type RanGen = PureMT

pureRanGen :: Word64 -> RanGen
pureRanGen = pureMT

newPureRanGen
  :: (MonadIO m)
  => m RanGen
newPureRanGen = liftIO newPureMT

class RanChoice a b | a -> b where
  pickRandom
    :: (MonadRandom m)
    => a -> m b

instance RanChoice [a] a where
  pickRandom xs =
    let maxInd = length xs - 1
    in unsafeIndex xs <$> getRandomR (0, maxInd)

instance RanChoice UUIDSet UUID where
  pickRandom us = pickRandom (unwrap us)
