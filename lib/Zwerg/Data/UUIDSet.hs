module Zwerg.Data.UUIDSet where

import Zwerg.Class
import Zwerg.Component.UUID
import Zwerg.Prelude hiding (toList, empty, filterM)

import qualified Control.Monad as CM (filterM)
import Data.Binary
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)

newtype UUIDSet =
  MkUUIDSet (Set UUID)
  deriving (Eq, Ord, Read, Show, Monoid, Generic)

instance Binary UUIDSet

instance ZWrapped UUIDSet (Set UUID) where
  unwrap (MkUUIDSet us) = us

instance ZContainer UUIDSet UUID where
  zAdd uuid (MkUUIDSet us) = MkUUIDSet $ S.insert uuid us
  zDelete uuid (MkUUIDSet us) = MkUUIDSet $ S.delete uuid us
  zMember uuid (MkUUIDSet us) = S.member uuid us

instance ZFilterable UUIDSet UUID where
  zFilter f (MkUUIDSet us) = MkUUIDSet $ S.filter f us
  zFilterM f (MkUUIDSet us) =
    MkUUIDSet <$> S.fromAscList <$> CM.filterM f (S.toAscList us)

instance ZEmptiable UUIDSet where
  zEmpty = MkUUIDSet S.empty
  zIsNull (MkUUIDSet us) = S.null us
  zSize (MkUUIDSet us) = S.size us

instance ZIsList UUIDSet UUID where
  zToList (MkUUIDSet us) = S.toAscList us
  zFromList = MkUUIDSet . S.fromList
