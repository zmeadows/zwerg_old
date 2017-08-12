module Zwerg.Random.Distribution
  ( Distribution(..)
  , sample
  ) where

import Zwerg.Prelude

import Data.Word (Word64)

import qualified Control.Monad.Random as CMR (MonadRandom, getRandom)
import qualified Data.Random.Distribution.Uniform as RU
import qualified Data.Random.RVar as RV


data Distribution
  = Uniform Double Double
  | Normal Double Double
  deriving (Generic)

instance Binary Distribution

--TODO: add more distributions and complete this function
sample :: forall m.  (CMR.MonadRandom m) => Distribution -> m Double
sample (Uniform low high) =
  RV.runRVar (RU.uniform low high) (CMR.getRandom :: m Word64)
sample (Normal low high) =
  RV.runRVar (RU.uniform low high) (CMR.getRandom :: m Word64)
