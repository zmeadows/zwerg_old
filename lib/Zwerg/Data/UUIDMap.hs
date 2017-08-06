module Zwerg.Data.UUIDMap
  ( UUIDMap
  , getMinimumUUIDs
  , toUUIDSet
  ) where

import Zwerg.Prelude

import Zwerg.Data.UUIDSet (UUIDSet)

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IM
import Data.Maybe (fromJust)

newtype UUIDMap a = MkUUIDMap (IntMap a)
    deriving (Show, Eq, Functor, Generic)

instance Binary a => Binary (UUIDMap a)

instance ZDefault (UUIDMap a) where
  zDefault = MkUUIDMap IM.empty

instance ZEmptiable (UUIDMap a) where
  zIsNull (MkUUIDMap m) = IM.null m
  zSize (MkUUIDMap m) = IM.size m

instance ZMapContainer (UUIDMap a) UUID a where
  zLookup uuid (MkUUIDMap m)     = IM.lookup (unwrap uuid) m
  zModify f uuid (MkUUIDMap m)   = MkUUIDMap $ IM.adjust f (unwrap uuid) m
  zInsert uuid val (MkUUIDMap m) = MkUUIDMap $ IM.insert (unwrap uuid) val m
  zRemoveAt uuid (MkUUIDMap m)   = MkUUIDMap $ IM.delete (unwrap uuid) m
  zContains uuid (MkUUIDMap m)   = IM.member (unwrap uuid) m
  zElems (MkUUIDMap m)           = IM.elems m
  zKeys (MkUUIDMap m)            = (catMaybes . map wrap) $ IM.keys m

instance ZIsList (UUIDMap a) (UUID, a) where
  zToList m = zip (zKeys m) (zElems m)
  zFromList kvs = MkUUIDMap $ IM.fromList $ map ( \(x,y) -> (unwrap x, y) ) kvs

instance ZFilterable (UUIDMap a) (UUID, a) where
    zFilter f (MkUUIDMap m) = MkUUIDMap $ IM.filterWithKey (\x -> curry f (fromJust $ wrap x)) m
    zFilterM f (MkUUIDMap m) = (MkUUIDMap . IM.fromAscList)
                               <$> (filterM (\(x,a) -> f (unsafeWrap x, a)) $ IM.toAscList m)

type instance IxValue (UUIDMap a) = a

-- TODO: just reference IntMap implementation?
-- TODO: try to derive?
-- FIXME: This could be causing slowdown?!?
instance Ixed (UUIDMap a) where
  ix k f m =
    case zLookup k m of
      Just v -> f v <&> \v' -> zInsert k v' m
      Nothing -> pure m

type instance Index (UUIDMap a) = UUID

-- TODO: just reference IntMap implementation?
-- TODO: try to derive?
-- FIXME: This could be causing slowdown?!?
instance At (UUIDMap a) where
  at k f m =
    f mv <&> \r ->
      case r of
        Nothing -> maybe m (const (zRemoveAt k m)) mv
        Just v' -> zInsert k v' m
    where
      mv = zLookup k m

getMinimumUUIDs :: (Ord a, Bounded a)
                => UUIDMap a -> (a, [UUID])
getMinimumUUIDs (MkUUIDMap um) =
  let (amin, ids) = IM.foldrWithKey f (minBound, []) um
  in (amin, ) $ map unsafeWrap ids
  where
    f uuid x (_, []) = (x, [uuid])
    f uuid x (xmin, uuids) =
      if | x == xmin -> (x, uuid : uuids)
         | x < xmin -> (x, [uuid])
         | otherwise -> (xmin, uuids)

toUUIDSet :: UUIDMap a -> UUIDSet
toUUIDSet m = unsafeWrap $ zKeys m
