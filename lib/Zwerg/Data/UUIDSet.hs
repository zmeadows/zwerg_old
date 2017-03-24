module Zwerg.Data.UUIDSet where

import Zwerg.Prelude hiding (toList, empty, filterM)
import Zwerg.Component.UUID
import Zwerg.Class

import Data.Binary
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import qualified Control.Monad as CM (filterM)

newtype UUIDSet = MkUUIDSet (Set UUID)
    deriving (Eq, Ord, Read, Show, Monoid, Generic)

instance Binary UUIDSet

instance ZWrapped UUIDSet (Set UUID) where
  {-# INLINABLE unwrap #-}
  unwrap (MkUUIDSet us) = us

instance ZContainer UUIDSet UUID where
  {-# INLINABLE zAdd #-}
  zAdd uuid (MkUUIDSet us) = MkUUIDSet $ S.insert uuid us
  {-# INLINABLE zDelete #-}
  zDelete uuid (MkUUIDSet us) = MkUUIDSet $ S.delete uuid us
  {-# INLINABLE zMember #-}
  zMember uuid (MkUUIDSet us) = S.member uuid us

instance ZFilterable UUIDSet UUID where
  {-# INLINABLE zFilter #-}
  zFilter f (MkUUIDSet us) = MkUUIDSet $ S.filter f us
  {-# INLINABLE zFilterM #-}
  zFilterM f (MkUUIDSet us)  = MkUUIDSet <$> S.fromAscList <$> CM.filterM f (S.toAscList us)

instance ZEmptiable UUIDSet where
  {-# INLINABLE zEmpty #-}
  zEmpty = MkUUIDSet S.empty
  zIsNull (MkUUIDSet us) = S.null us

instance ZIsList UUIDSet UUID where
  zToList (MkUUIDSet us) = S.toAscList us
  zFromList = MkUUIDSet . S.fromList
