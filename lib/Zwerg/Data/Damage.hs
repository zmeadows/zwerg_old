module Zwerg.Data.Damage where

import Zwerg.Prelude
import Zwerg.Random.Distribution

data DamageAttribute
  = Fire
  | Ice
  | Poison
  | Electricity
  | Pierce
  | Bludgeon
  | Slash
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary DamageAttribute

data DamageData = DamageData
  { _damageDataTargetType :: TargetType
  , _damageDataAttribute :: DamageAttribute
  , _damageDataDistribution :: Distribution
  } deriving (Show, Read, Eq, Generic)

makeFields ''DamageData

instance Binary DamageData

type DamageChain = [DamageData]
