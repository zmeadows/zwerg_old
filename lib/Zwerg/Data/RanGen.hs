module Zwerg.Data.RanGen (RanGen, pureRanGen) where

import System.Random.Mersenne.Pure64 (PureMT, pureMT)
import Data.Word (Word64)

type RanGen = PureMT

pureRanGen :: Word64 -> RanGen
pureRanGen = pureMT
