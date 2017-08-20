{-# LANGUAGE AllowAmbiguousTypes #-}

module Zwerg.Prelude.Class
  ( ZWrapped(..)
  , unsafeWrap
  , wrapOrDefault
  , ZIsList(..)
  , ZDefault(..)
  , ZEmptiable(..)
  , ZSetContainer(..)
  , ZMapContainer(..)
  , ZIncompleteMapContainer(..)
  , ZCompleteMapContainer(..)
  , ZFilterable(..)
  , ZTraversable2(..)
  , zTraverseWithKey_
  ) where

import Prelude

import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (void)
import Data.Text (Text)

class ZWrapped a b | a -> b where
  unwrap :: a -> b
  wrap   :: b -> Maybe a

wrapOrDefault :: (ZDefault a, ZWrapped a b) => b -> a
wrapOrDefault x = fromMaybe zDefault (wrap x)

-- primarily for use in conversions between newtypes or rare cases when
-- we KNOW we cannot fail the wrapping condition (example: convert UUIDMap keys to a UUIDSet)
unsafeWrap :: ZWrapped a b => b -> a
unsafeWrap = fromJust . wrap

class ZDefault t where
  zDefault :: t

instance ZDefault Text where
  zDefault = "DEFAULT"
instance ZDefault Int where
  zDefault = 1
instance ZDefault Char where
  zDefault = '⸮'
instance ZDefault Bool where
  zDefault = False
instance ZDefault [a] where
  zDefault = []

class ZEmptiable a where
  zIsNull :: a -> Bool
  zSize   :: a -> Int

class ZIsList f where
  zToList :: f a -> [a]

class (ZEmptiable s) => ZSetContainer s v | s -> v where
  zAdd    :: v -> s -> s
  zDelete :: v -> s -> s
  zMember :: v -> s -> Bool

class ZMapContainer f k | f -> k where
  zModifyAt :: (a -> a) -> k -> f a -> f a
  zElems    :: f a -> [a]

class (ZMapContainer f k) => ZIncompleteMapContainer f k | f -> k where
  zLookup   :: k -> f a -> Maybe a
  zInsert   :: k -> a -> f a -> f a
  zRemoveAt :: k -> f a -> f a
  zContains :: k -> f a -> Bool
  zKeys     :: f a -> [k]

class (ZMapContainer f k) => ZCompleteMapContainer f k | f -> k where
  zAt      :: f a -> k -> a
  zIndices :: [k]
  zBuild   :: (k -> a) -> f a
  zBuildM  :: Monad m => (k -> m a) -> m (f a)

class (ZMapContainer f k) => ZTraversable2 f k | f -> k where
  zTraverseWithKey :: Monad m => f a -> (k -> a -> m b) -> m (f b)

zTraverseWithKey_ :: (ZTraversable2 f k, Monad m)
                  => f a -> (k -> a -> m ()) -> m ()
zTraverseWithKey_ g f = void $ zTraverseWithKey g f

class ZFilterable a b | a -> b where
  zFilter  :: (b -> Bool) -> a -> a
  zFilterM :: (Monad m) => (b -> m Bool) -> a -> m a

