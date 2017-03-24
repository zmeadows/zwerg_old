module Zwerg.Data.UUIDMap (UUIDMap(..), Zwerg.Component.UUID.UUID) where

import Zwerg.Prelude
import Zwerg.Component.UUID
import Zwerg.Class

import Control.Lens (At(..), Ixed(..), Index, IxValue, (<&>))
import Data.Binary
import GHC.Generics (Generic)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM

newtype UUIDMap a = MkUUIDMap (IntMap a)
    deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable, Monoid, Generic)

instance Binary a => Binary (UUIDMap a)

instance ZEmptiable (UUIDMap a) where
  {-# INLINABLE zEmpty #-}
  zEmpty = MkUUIDMap IM.empty
  {-# INLINABLE zIsNull #-}
  zIsNull (MkUUIDMap um) = IM.size um == 0

instance ZWrapped (UUIDMap a) (IntMap a) where
  unwrap (MkUUIDMap um) = um

instance ZIsList (UUIDMap a) (Int,a) where
  zToList (MkUUIDMap um) = IM.toList um
  zFromList = MkUUIDMap . IM.fromList

instance ZMapContainer (UUIDMap a) UUID a where
  {-# INLINABLE zLookup #-}
  zLookup uuid (MkUUIDMap m) = IM.lookup (unwrap uuid) m
  {-# INLINABLE zAdjust #-}
  zAdjust f uuid (MkUUIDMap m) = MkUUIDMap $ IM.adjust f (unwrap uuid) m
  {-# INLINABLE zInsert #-}
  zInsert uuid x (MkUUIDMap m) = MkUUIDMap $ IM.insert (unwrap uuid) x m
  {-# INLINABLE zRemoveAt #-}
  zRemoveAt uuid (MkUUIDMap m) = MkUUIDMap $ IM.delete (unwrap uuid) m
  {-# INLINABLE zContains #-}
  zContains uuid (MkUUIDMap m) = IM.member (unwrap uuid) m
   
instance ZFilterable (UUIDMap a) (Int,a) where
  {-# INLINABLE zFilter #-}
  zFilter f (MkUUIDMap m) = MkUUIDMap $ IM.filterWithKey (\i x -> f (i,x)) m
  {-# INLINABLE zFilterM #-}
  zFilterM f (MkUUIDMap m) =  MkUUIDMap <$> IM.fromAscList <$> filterM f (IM.toAscList m)

type instance IxValue (UUIDMap a) = a

instance Ixed (UUIDMap a) where
  {-# INLINABLE ix #-}
  ix k f m = case zLookup k m of
     Just v -> f v <&> \v' -> zInsert k v' m
     Nothing -> pure m

type instance Index (UUIDMap a) = UUID

instance At (UUIDMap a) where
  {-# INLINABLE at #-}
  at k f m = f mv <&> \r -> case r of
    Nothing -> maybe m (const (zRemoveAt k m)) mv
    Just v' -> zInsert k v' m
    where mv = zLookup k m

