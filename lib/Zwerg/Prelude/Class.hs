module Zwerg.Prelude.Class
  ( ZWrapped(..)
  , ZIsList(..)
  , ZEmptiable(..)
  , ZContainer(..)
  , ZMapContainer(..)
  , ZFilterable(..)
  ) where

import Protolude

-- import Control.Monad.Random.Class as EXPORTED
-- import Data.Binary as EXPORTED (Binary)
-- import Data.Monoid as EXPORTED ((<>))
-- import Data.Text as EXPORTED (Text, pack, unpack)
-- import Data.Traversable as EXPORTED (forM)
-- import GHC.Generics as EXPORTED (Generic)
-- import Lens.Micro.Internal as EXPORTED
--        (At(..), Ixed(..), Index, IxValue)
-- import Lens.Micro.Platform as EXPORTED
--        (makeClassy, makeLenses, makeFields, (%=), (^.), (.=), over, use,
--         view, to, set, Lens', (<&>))

class ZWrapped a b | a -> b where
  unwrap :: a -> b

class ZIsList a b | a -> b where
  zToList   :: a -> [b]
  zFromList :: [b] -> a

class ZEmptiable a where
  zEmpty  :: a
  zIsNull :: a -> Bool
  zSize   :: a -> Int

class ZContainer a b | a -> b where
  zAdd    :: b -> a -> a
  zDelete :: b -> a -> a
  zMember :: b -> a -> Bool

class ZMapContainer a b c | a -> b c where
  zLookup   :: b -> a -> Maybe c
  zAdjust   :: (c -> c) -> b -> a -> a
  zInsert   :: b -> c -> a -> a
  zRemoveAt :: b -> a -> a
  zContains :: b -> a -> Bool

class ZFilterable a b | a -> b where
  zFilter  :: (b -> Bool) -> a -> a
  zFilterM :: (Monad m) => (b -> m Bool) -> a -> m a
