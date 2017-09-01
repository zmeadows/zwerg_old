module Zwerg.Data.Damage
  ( DamageAttribute(..)
  , DamageData(..)
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
  deriving stock Generic
  deriving anyclass Binary

instance ZDefault DamageAttribute where
    zDefault = Slash

data DamageData = DamageData
    { ddTargetType   :: TargetType
    , ddAttribute    :: DamageAttribute
    , ddDistribution :: Distribution }
  deriving stock Generic
  deriving anyclass Binary

instance ZDefault DamageData where
    zDefault = DamageData SingleTarget zDefault $ Uniform 0 0

type DamageChain = [DamageData]


newtype Resistance = MkResistance Int
    deriving stock Generic
    deriving anyclass Binary

newtype Resistances = MkResistances (Map DamageAttribute Resistance)
    deriving stock Generic
    deriving anyclass Binary

