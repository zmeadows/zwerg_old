module Zwerg.Random.Distribution
  ( Distribution
  , sample
  ) where

import Zwerg.Prelude

import qualified Control.Monad.Random as CMR
       (MonadRandom, getRandom)
import Data.Binary
import qualified Data.Random.Distribution.Uniform as RU
import qualified Data.Random.RVar as RV
import GHC.Generics (Generic)

data Distribution
  = Uniform Double
            Double
  | Normal Double
           Double
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary Distribution

sample
  :: forall m.
     (CMR.MonadRandom m)
  => Distribution -> m Double
sample (Uniform low high) =
  RV.runRVar (RU.uniform low high) (CMR.getRandom :: m Word64)
sample (Normal low high) =
  RV.runRVar (RU.uniform low high) (CMR.getRandom :: m Word64)
-- uniform
--   :: RD.Distribution RU.Uniform a
--   => a -> a -> Distribution a
-- uniform low high = MkDistribution (uniform' low high)
