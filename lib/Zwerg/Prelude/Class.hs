module Zwerg.Prelude.Class
  ( ZWrapped(..)
  , unsafeWrap
  , wrapOrDefault
  , ZIsList(..)
  , ZDefault(..)
  , ZEmptiable(..)
  , ZSetContainer(..)
  , ZCompleteMapContainer(..)
  , ZMapContainer(..)
  , ZFilterable(..)
  , ZTraversable2(..)
  , zTraverseWithKey_
  ) where

import Prelude

import Data.Maybe (fromJust)
import Control.Monad (void)
import Data.Text (Text)

class ZWrapped wrapped unwrapped | wrapped -> unwrapped where
  unwrap :: wrapped -> unwrapped
  wrap   :: unwrapped -> Maybe wrapped

wrapOrDefault :: (ZDefault a, ZWrapped a b) => b -> a
wrapOrDefault x = case wrap x of
                    Just y -> y
                    Nothing -> zDefault

-- primarily for use in conversions between newtypes or rare cases when
-- we KNOW we cannot fail the wrapping condition (example: convert UUIDMap keys to a UUIDSet)
unsafeWrap :: ZWrapped a b => b -> a
unsafeWrap = fromJust . wrap

class ZIsList listlike itemtype | listlike -> itemtype where
  zToList   :: listlike -> [itemtype]
  zFromList :: [itemtype] -> listlike

class ZDefault t where
  zDefault :: t

instance ZDefault Text where
  zDefault = "defaultText"

instance ZDefault Int where
  zDefault = 1

instance ZDefault Bool where
  zDefault = False

instance ZDefault [a] where
    zDefault = []

class ZEmptiable container where
  zIsNull :: container -> Bool
  zSize   :: container -> Int

class (ZEmptiable container) => ZSetContainer container itemtype | container -> itemtype where
  zAdd    :: itemtype -> container -> container
  zDelete :: itemtype -> container -> container
  zMember :: itemtype -> container -> Bool

--TODO: use more descriptive type variables than a b c
class (ZEmptiable a) => ZMapContainer a b c | a -> b c where
  zLookup   :: b -> a -> Maybe c
  zInsert   :: b -> c -> a -> a
  zRemoveAt :: b -> a -> a
  zModify   :: (c -> c) -> b -> a -> a
  zContains :: b -> a -> Bool
  zElems    :: a -> [c]
  zKeys     :: a -> [b]

class ZCompleteMapContainer container key value | container -> key value where
  zAt     :: container -> key -> value
  zAdjust :: (value -> value) -> key -> container -> container
  zBuild  :: (key -> value) -> container
  zBuildM :: Monad m => (key -> m value) -> m container

class ZTraversable2 container key | container -> key where
  zTraverseWithKey :: Monad m => container a -> (key -> a -> m b) -> m (container b)

zTraverseWithKey_ :: (ZTraversable2 t k, Monad m)
                  => t a -> (k -> a -> m ()) -> m ()
zTraverseWithKey_ g f = void $ zTraverseWithKey g f

class ZFilterable a b | a -> b where
  zFilter  :: (b -> Bool) -> a -> a
  zFilterM :: (Monad m) => (b -> m Bool) -> a -> m a

--TODO: ZFilterableToList ?

