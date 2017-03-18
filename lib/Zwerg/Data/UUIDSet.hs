module Zwerg.Data.UUIDSet (
    UUIDSet,
    empty,
    member,
    insert,
    uuidSetToList
    ) where

import Zwerg.Component.UUID

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import GHC.Generics (Generic)
import Data.Binary

newtype UUIDSet = MkUUIDSet IntSet
    deriving (
        Eq,
        Ord,
        Read,
        Show,
        Monoid,
        Generic
        )

instance Binary UUIDSet

{-# INLINABLE empty #-}
empty :: UUIDSet
empty = MkUUIDSet IS.empty

{-# INLINABLE member #-}
member :: UUID -> UUIDSet -> Bool
member uuid (MkUUIDSet us) = IS.member (unUUID uuid) us

{-# INLINABLE insert #-}
insert :: UUID -> UUIDSet -> UUIDSet
insert uuid (MkUUIDSet us) = MkUUIDSet $ IS.insert (unUUID uuid) us

uuidSetToList :: UUIDSet -> [UUID]
uuidSetToList (MkUUIDSet us) = map mkUUID $ IS.toList us

-- {-# INLINABLE delete #-}
-- delete :: UUID -> UUIDSet -> UUIDSet
-- delete uuid (MkUUIDSet us) = MkUUIDSet $ IS.delete (unUUID uuid) us
