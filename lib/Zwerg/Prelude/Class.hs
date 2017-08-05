module Zwerg.Prelude.Class
  ( ZWrapped(..)
  , ZIsList(..)
  , ZDefault(..)
  , ZEmptiable(..)
  , ZSetContainer(..)
  , ZCompleteMapContainer(..)
  , ZMapContainer(..)
  , ZFilterable(..)
  , ZBuildable(..)
  , ZTraversable2(..)
  , zTraverseWithKey_
  ) where

import Prelude (Bool, Monad, Int, Maybe, Applicative, Monad, ($), (<$>), filter)
import Control.Monad (filterM, void)

class ZWrapped wrapped unwrapped | wrapped -> unwrapped where
  unwrap :: wrapped -> unwrapped
  wrap   :: unwrapped -> Maybe wrapped

class ZIsList listlike itemtype | listlike -> itemtype where
  zToList   :: listlike -> [itemtype]
  zFromList :: [itemtype] -> listlike

class ZDefault t where
  zDefault :: t

class ZEmptiable container where
  zIsNull :: container -> Bool
  zSize   :: container -> Int

class ZSetContainer container itemtype | container -> itemtype where
  zAdd    :: itemtype -> container -> container
  zDelete :: itemtype -> container -> container
  zMember :: itemtype -> container -> Bool

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

class (ZCompleteMapContainer t k v) => ZBuildable t k v | t -> k v where
  zBuild  :: (k -> v) -> t
  zBuildM :: (k -> m v) -> m t

class ZTraversable2 container key | container -> key where
  zTraverseWithKey :: Applicative t => container a -> (key -> a -> t b) -> t (container b)

zTraverseWithKey_ :: (ZTraversable2 t k, Applicative t)
                  => t a -> (k -> a -> t ()) -> t ()
zTraverseWithKey_ g f = void $ zTraverseWithKey g f

class ZIsList a b => ZFilterable a b | a -> b where
  zFilter  :: (b -> Bool) -> a -> a
  zFilter f m = zFromList $ filter f $ zToList m
  zFilterM :: (Monad m) => (b -> m Bool) -> a -> m a
  zFilterM f m = zFromList <$> filterM f (zToList m)

