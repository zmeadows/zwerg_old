module Zwerg.Data.UUIDSet where

import Zwerg.Prelude

import qualified Control.Monad as CM (filterM, liftM)
import qualified Data.List as L (delete)

newtype UUIDSet =
  MkUUIDSet [UUID]
  deriving (Eq, Ord, Read, Show, Monoid, Generic)

instance Binary UUIDSet

instance ZWrapped UUIDSet [UUID] where
  unwrap (MkUUIDSet us) = us

instance ZContainer UUIDSet UUID where
  zAdd uuid (MkUUIDSet us) =
    if uuid `elem` us
       then MkUUIDSet us
       else MkUUIDSet (uuid:us)
  zDelete uuid (MkUUIDSet us) = MkUUIDSet $ L.delete uuid us
  zMember uuid (MkUUIDSet us) = uuid `elem` us

instance ZFilterable UUIDSet UUID where
  zFilter f (MkUUIDSet us) = MkUUIDSet $ filter f us
  zFilterM f (MkUUIDSet us) = CM.liftM MkUUIDSet $ CM.filterM f us

instance ZEmptiable UUIDSet where
  zEmpty = MkUUIDSet []
  zIsNull (MkUUIDSet us) = null us
  zSize (MkUUIDSet us) = length us

instance ZIsList UUIDSet UUID where
  zToList (MkUUIDSet us) = us
  zFromList = MkUUIDSet
