module Zwerg.Data.UUIDSet where

import Zwerg.Prelude

import qualified Control.Monad as CM (filterM)
import qualified Data.List as L (delete, nub)

newtype UUIDSet = MkUUIDSet [UUID]
  deriving (Eq, Ord, Show, Monoid, Semigroup, Generic)

instance Binary UUIDSet

instance ZWrapped UUIDSet [UUID] where
  unwrap (MkUUIDSet us) = us
  wrap uuids = if | (length $ L.nub uuids) == length uuids -> Just $ MkUUIDSet uuids
                  | otherwise -> Nothing

instance ZSetContainer UUIDSet UUID where
  zAdd uuid (MkUUIDSet us) =
    if uuid `elem` us
       then MkUUIDSet us
       else MkUUIDSet (uuid:us)
  zDelete uuid (MkUUIDSet us) = MkUUIDSet $ L.delete uuid us
  zMember uuid (MkUUIDSet us) = uuid `elem` us

instance ZFilterable UUIDSet UUID where
  zFilter f (MkUUIDSet us) = MkUUIDSet $ filter f us
  zFilterM f (MkUUIDSet us) = MkUUIDSet <$> CM.filterM f us

instance ZEmptiable UUIDSet where
  zIsNull (MkUUIDSet us) = null us
  zSize (MkUUIDSet us) = length us

instance ZDefault UUIDSet where
    zDefault = MkUUIDSet []
