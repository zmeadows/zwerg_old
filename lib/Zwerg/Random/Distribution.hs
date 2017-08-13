module Zwerg.Random.Distribution
  ( Distribution(..)
  , sample
  ) where

import Zwerg.Prelude

import qualified Control.Monad.Random as CMR (MonadRandom)

data Distribution
  = Uniform Double Double
  | Normal Double Double
  deriving (Generic)

instance Binary Distribution

--TODO: add more distributions and complete this function
sample :: forall m. (CMR.MonadRandom m) => Distribution -> m Double
sample (Uniform low high) = getRandomR (low, high)
--TODO: fix this
sample (Normal low high) = getRandomR (low, high)

