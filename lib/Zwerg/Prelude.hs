module Zwerg.Prelude
  ( module EXPORTED
  , UUID
  , ZConstructable(..)
  , ZWrapped(..)
  , TargetType
  ) where

import Control.Lens as EXPORTED
       (makeClassy, makeLenses, (%=), (^.), (.=), over, use, view, to,
        Lens')
import Control.Monad.Random.Class as EXPORTED
import Data.Binary as EXPORTED (Binary)
import Data.Text as EXPORTED (Text, pack, unpack)
import Data.Traversable as EXPORTED (forM)
import GHC.Generics as EXPORTED (Generic)
import Protolude as EXPORTED hiding (to, forM, (<>))

mapWidthDOUBLE :: Double
mapWidthDOUBLE = 100

mapHeightDOUBLE :: Double
mapHeightDOUBLE = 25

mapWidthINT :: Int
mapWidthINT = round mapWidthDOUBLE

mapHeightINT :: Int
mapHeightINT = round mapHeightDOUBLE

class ZConstructable a b | a -> b where
  zConstruct
    :: (MonadError ZError m)
    => b -> m a

class ZWrapped a b | a -> b where
  unwrap :: a -> b

class ZIsList a b | a -> b where
  zToList :: a -> [b]
  zFromList :: [b] -> a

class ZEmptiable a where
  zEmpty :: a
  zIsNull :: a -> Bool
  zSize :: a -> Int

class ZContainer a b | a -> b where
  zAdd :: b -> a -> a
  zDelete :: b -> a -> a
  zMember :: b -> a -> Bool

class ZMapContainer a b c | a -> b c where
  zLookup :: b -> a -> Maybe c
  zAdjust :: (c -> c) -> b -> a -> a
  zInsert :: b -> c -> a -> a
  zRemoveAt :: b -> a -> a
  zContains :: b -> a -> Bool

class ZFilterable a b | a -> b where
  zFilter :: (b -> Bool) -> a -> a
  zFilterM
    :: (Monad m)
    => (b -> m Bool) -> a -> m a

data ZErrorLevel
  = Warning
  | Fatal
  deriving (Show, Read, Eq)

data ZError = ZError
  { _file :: Text
  , _line :: Int
  , _errLevel :: ZErrorLevel
  , _description :: Text
  } deriving (Show, Read, Eq)

makeClassy ''ZError

data Direction
  = North
  | South
  | East
  | West
  | NorthWest
  | NorthEast
  | SouthWest
  | SouthEast
  deriving (Show, Read, Eq, Generic)

instance Binary Direction

data TargetType
  = SingleTarget
  | AOE Double
  | Line Direction
         Int
  deriving (Show, Read, Eq, Generic)

instance Binary TargetType

data Glyph = Glyph
  { char :: Char
  , fgColorVIS :: Word8
  , fgColorFOG :: Word8
  , bgColorVIS :: Maybe Word8
  , bgColorFOG :: Maybe Word8
  } deriving (Show, Read, Eq, Generic)

instance Binary Glyph

data EntityType
  = Player
  | Enemy
  | Item
  | Tile
  | Level
  | Container
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary EntityType

newtype UUID =
  MkUUID Int
  deriving (Show, Read, Eq, Bounded, Enum, Num, Ord, Generic)

instance Binary UUID

instance ZWrapped UUID Int where
  unwrap (MkUUID uuid) = uuid

playerUUID :: UUID
playerUUID = MkUUID 0
