module Zwerg.Data.Damage
  ( DamageAttribute(..)
  , DamageData(..)
  , HasTargetType(..)
  , HasAttribute(..)
  , HasDistribution(..)
  , DamageChain
  , Resistance(..)
  , Resistances
  ) where

import Zwerg.Prelude
import Zwerg.Random.Distribution

import Data.Map (Map)

data DamageAttribute
  = Fire
  | Ice
  | Poison
  | Electricity
  | Slash
  | Pierce
  | Bludgeon
  | Holy
  | Evil
  deriving (Generic)

instance Binary DamageAttribute

instance ZDefault DamageAttribute where
    zDefault = Slash

data DamageData = DamageData
  { _damageDataTargetType   :: TargetType
  , _damageDataAttribute    :: DamageAttribute
  , _damageDataDistribution :: Distribution
  } deriving (Generic)
makeFields ''DamageData
instance Binary DamageData

instance ZDefault DamageData where
    zDefault = DamageData SingleTarget zDefault $ Uniform 0 0

type DamageChain = [DamageData]

--TODO: make newtype of Int
newtype Resistance = MkResistance Int
  deriving (Generic)
instance Binary Resistance

-- increaseResistance :: Resistance -> Int -> Resistance
-- increaseResistance (MkResistance r) i = MkResistance $ min 10 $ r + i

-- decreaseResistance :: Resistance -> Int -> Resistance
-- decreaseResistance (MkResistance r) i = MkResistance $ max (-10) $ r - i

newtype Resistances = MkResistances (Map DamageAttribute Resistance)
  deriving (Generic)
instance Binary Resistances


