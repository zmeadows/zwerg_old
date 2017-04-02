module Zwerg.Data.Damage where

import Zwerg.Data.Target
import Zwerg.Prelude
import Zwerg.Random.Distribution

import qualified Data.Set as S (fromList)

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
