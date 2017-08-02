module Zwerg.Data.UUIDMap
  ( UUIDMap(..)
  , NamedUUIDMap(..)
  , HasNamedUUIDMap(..)
  , getMinimumUUIDs
  ) where

import Zwerg.Prelude

--TODO: don't export constructors for newtypes

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
  ( adjust
  , insert
  , delete
  , filterWithKey
  , fromAscList
  , toAscList
  , empty
  , size
  , lookup
  , member
  , foldrWithKey
  )

newtype UUIDMap a = MkUUIDMap (IntMap a)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Monoid, Semigroup, Generic)
instance Binary a => Binary (UUIDMap a)

--TODO: switch to newtype tuple above, no need for new data type
data NamedUUIDMap a = NamedUUIDMap
  { _componentName :: Text
  , _uuidMap :: UUIDMap a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)
makeClassy ''NamedUUIDMap
instance Binary a => Binary (NamedUUIDMap a)

instance ZEmptiable (UUIDMap a) where
  zEmpty = MkUUIDMap IM.empty
  zIsNull (MkUUIDMap um) = IM.size um == 0
  zSize (MkUUIDMap um) = IM.size um

instance ZWrapped (UUIDMap a) (IntMap a) where
  unwrap (MkUUIDMap um) = um

instance ZIsList (UUIDMap a) (Int, a) where
  zToList (MkUUIDMap um) = toList um
  zFromList = MkUUIDMap . fromList

-- TODO: remove these classes... they dillute the point of newtype?
instance ZMapContainer (UUIDMap a) UUID a where
  zLookup uuid (MkUUIDMap m) = IM.lookup (unwrap uuid) m
  zAdjust f uuid (MkUUIDMap m) = MkUUIDMap $ IM.adjust f (unwrap uuid) m
  zInsert uuid x (MkUUIDMap m) = MkUUIDMap $ IM.insert (unwrap uuid) x m
  zRemoveAt uuid (MkUUIDMap m) = MkUUIDMap $ IM.delete (unwrap uuid) m
  zContains uuid (MkUUIDMap m) = IM.member (unwrap uuid) m

--TODO: (UUID,a) instead of (Int,a)?
instance ZFilterable (UUIDMap a) (Int, a) where
  zFilter f (MkUUIDMap m) = MkUUIDMap $ IM.filterWithKey (curry f) m
  zFilterM f (MkUUIDMap m) = MkUUIDMap . IM.fromAscList <$> filterM f (IM.toAscList m)

type instance IxValue (UUIDMap a) = a

-- TODO: just reference IntMap implementation?
-- FIXME: This could be causing slowdown?!?
instance Ixed (UUIDMap a) where
  ix k f m =
    case zLookup k m of
      Just v -> f v <&> \v' -> zInsert k v' m
      Nothing -> pure m

type instance Index (UUIDMap a) = UUID

-- TODO: just reference IntMap implementation?
-- FIXME: This could be causing slowdown?!?
instance At (UUIDMap a) where
  at k f m =
    f mv <&> \r ->
      case r of
        Nothing -> maybe m (const (zRemoveAt k m)) mv
        Just v' -> zInsert k v' m
    where
      mv = zLookup k m

getMinimumUUIDs :: (Ord a, Bounded a, MonadError ZError m)
                => UUIDMap a -> m (a, [UUID])
getMinimumUUIDs (MkUUIDMap um) =
  let (amin, ids) = IM.foldrWithKey f (minBound, []) um
  in liftM (amin, ) $ mapM zConstruct ids
  where
    f uuid x (_, []) = (x, [uuid])
    f uuid x (xmin, uuids) =
      if | x == xmin -> (x, uuid : uuids)
         | x < xmin -> (x, [uuid])
         | otherwise -> (xmin, uuids)
