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
  { targetType :: TargetType
  , attribute :: DamageAttribute
  , distribution :: Distribution
  } deriving (Show, Read, Eq, Generic)

instance Binary DamageData

type DamageChain = [DamageData]
