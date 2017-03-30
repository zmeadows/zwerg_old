module Zwerg.Data.Damage where

import Zwerg.Data.Target
import Zwerg.Prelude
import Zwerg.Random.Distribution

import Data.Binary
import qualified Data.Set as S (fromList)
import GHC.Generics (Generic)

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
  { target :: TargetType
  , attribute :: DamageAttribute
  , distribution :: Distribution
  } deriving (Show, Read, Eq, Generic)

instance Binary DamageData

type DamageChain = [DamageData]

elementalDamageAttributes :: Set DamageAttribute
elementalDamageAttributes = S.fromList [Fire, Ice]

physicalDamageAttributes :: Set DamageAttribute
physicalDamageAttributes = S.fromList [Pierce, Bludgeon, Slash]
