module Zwerg.Data.UUIDMap (
    UUIDMap,
    UUID,
    empty,
    lookup,
    delete,
    insert,
    member,
    adjust,
    filter
    ) where

import Prelude hiding (
    lookup,
    filter
    )

import Zwerg.Component.UUID (UUID, unUUID)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM (
    empty,
    lookup,
    delete,
    insert,
    member,
    adjust,
    filter
    )

import Control.Lens (
    At(..),
    Ixed(..),
    Index,
    IxValue,
    (<&>)
    )

import Data.Binary
import GHC.Generics (Generic)

newtype UUIDMap a = MkUUIDMap (IntMap a)
    deriving (
        Functor,
        Foldable,
        Traversable,
        Monoid,
        Generic,
        Show, Read, Eq, Ord
    )

instance Binary a => Binary (UUIDMap a)

type instance IxValue (UUIDMap a) = a

instance Ixed (UUIDMap a) where
  {-# INLINABLE ix #-}
  ix k f m = case lookup k m of
     Just v -> f v <&> \v' -> insert k v' m
     Nothing -> pure m

type instance Index (UUIDMap a) = UUID

instance At (UUIDMap a) where
  {-# INLINABLE at #-}
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
    where mv = lookup k m

{-# INLINABLE empty #-}
empty :: UUIDMap a
empty = MkUUIDMap IM.empty

{-# INLINABLE lookup #-}
lookup :: UUID -> UUIDMap a -> Maybe a
lookup uuid (MkUUIDMap m) = IM.lookup (unUUID uuid) m

{-# INLINABLE delete #-}
delete :: UUID -> UUIDMap a -> UUIDMap a
delete uuid (MkUUIDMap m) = MkUUIDMap $ IM.delete (unUUID uuid) m

{-# INLINABLE insert #-}
insert :: UUID -> a -> UUIDMap a -> UUIDMap a
insert uuid x (MkUUIDMap m) = MkUUIDMap $ IM.insert (unUUID uuid) x m

{-# INLINABLE member #-}
member :: UUID -> UUIDMap a -> Bool
member uuid (MkUUIDMap m) = IM.member (unUUID uuid) m

{-# INLINABLE adjust #-}
adjust :: (a -> a) -> UUID -> UUIDMap a -> UUIDMap a
adjust f uuid (MkUUIDMap m) = MkUUIDMap $ IM.adjust f (unUUID uuid) m

{-# INLINABLE filter #-}
filter :: (a -> Bool) -> UUIDMap a -> UUIDMap a
filter f (MkUUIDMap m) = MkUUIDMap $ IM.filter f m
