module Zwerg.Data.Damage
  ( DamageAttribute(..)
  , DamageData(..)
  , targetType
  , attribute
  , distribution
  , DamageChain
  , ResistanceLevel(..)
  , Resistances
  ) where

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
  | Holy
  | Evil
  deriving (Show, Eq, Ord, Bounded, Generic)

instance Binary DamageAttribute

data DamageData = DamageData
  { _damageDataTargetType   :: TargetType
  , _damageDataAttribute    :: DamageAttribute
  , _damageDataDistribution :: Distribution
  } deriving (Show, Eq, Generic)
makeFields ''DamageData
instance Binary DamageData

type DamageChain = [DamageData]

data ResistanceLevel
  = Weak
  | Medium
  | Strong
  | Complete
  | WeakHeal
  | MediumHeal
  | StrongHeal
  deriving (Show, Eq, Ord, Bounded, Generic)
instance Binary ResistanceLevel

newtype Resistances = MkResistances (Map DamageAttribute ResistanceLevel)
  deriving (Show, Eq, Ord, Generic)
instance Binary Resistances




