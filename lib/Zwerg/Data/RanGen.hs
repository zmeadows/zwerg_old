module Zwerg.Data.RanGen (RanGen, newPureRanGen, pureRanGen) where

import System.Random.Mersenne.Pure64 (PureMT, pureMT, newPureMT)
import Data.Word (Word64)
import Control.Monad.IO.Class (MonadIO, liftIO)

type RanGen = PureMT

pureRanGen :: Word64 -> RanGen
pureRanGen = pureMT

newPureRanGen :: (MonadIO m) => m RanGen
newPureRanGen = liftIO newPureMT
