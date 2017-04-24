module Zwerg.Prelude
  ( module EXPORTED
  , ZConstructable(..)
  , ZWrapped(..)
  , ZIsList(..)
  , ZEmptiable(..)
  , ZContainer(..)
  , ZMapContainer(..)
  , ZFilterable(..)
  , ZErrorLevel(..)
  , ZError(..)
  , Direction(..)
  , TargetType(..)
  , Color(..)
  , Glyph(..)
  , EntityType(..)
  , UUID
  , playerUUID
  , Parent(..)
  , TileType(..)
  , Stat(..)
  , Stats(..)
  , zeroStats
  , lookupStat
  , replaceStat
  , EquipmentSlot
  , WeaponSlot(..)
  , ArmorSlot(..)
  , EquippableSlot(..)
  , Equipment(..)
  , emptyEquipment
  , equip
  , unequip
  , getEquippedInSlot
  , getAllEquippedItems
  , AIType(..)
  , ItemType(..)
  , mapWidthDOUBLE
  , mapWidthINT
  , mapHeightDOUBLE
  , mapHeightINT
  ) where

import Protolude as EXPORTED hiding (to, forM, (<>))

import Control.Monad.Random.Class as EXPORTED
import Data.Binary as EXPORTED (Binary)
import Data.Monoid as EXPORTED ((<>))
import Data.Text as EXPORTED (Text, pack, unpack)
import Data.Traversable as EXPORTED (forM)
import GHC.Generics as EXPORTED (Generic)
import Lens.Micro.Internal as EXPORTED
       (At(..), Ixed(..), Index, IxValue)
import Lens.Micro.Platform as EXPORTED
       (makeClassy, makeLenses, makeFields, (%=), (^.), (.=), over, use,
        view, to, set, Lens', (<&>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-------------
-- CLASSES --
-------------
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

data Color
  = Green0
  | Green1
  | Green2
  | Green3
  | Blue0
  | Blue1
  | Blue2
  | Blue3
  | Red0
  | Red1
  | Red2
  | Red3
  | Black0
  | Black1
  | Black2
  | Black3
  | White0
  | White1
  | White2
  | White3
  deriving (Eq, Read, Show, Generic)

instance Binary Color

data Glyph = Glyph
  { char :: Char
  , fgColorVIS :: Color
  , fgColorFOG :: Color
  , bgColorVIS :: Maybe Color
  , bgColorFOG :: Maybe Color
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

instance ZConstructable UUID Int where
  zConstruct x =
    if | x >= 0 -> return $ MkUUID x
       | otherwise ->
         throwError $
         ZError __FILE__ __LINE__ Fatal "Attempted to construct UUID < 0"

playerUUID :: UUID
playerUUID = MkUUID 0

data Parent
  = Alive UUID
  | Dead
  deriving (Show, Read, Eq, Generic)

instance Binary Parent

data TileType
  = Floor
  | Wall
  | Door
  | Void
  deriving (Show, Read, Eq, Generic)

instance Binary TileType

data Stat
  = STR
  | DEX
  | INT
  | CHA
  | CON
  | WIS
  deriving (Read, Show, Eq, Ord, Enum, Generic)

instance Binary Stat

newtype Stats =
  MkStats (Map Stat Int)
  deriving (Show, Read, Eq, Generic)

instance Binary Stats

zeroStats :: Stats
zeroStats = MkStats $ M.fromList $ fmap (, 0) $ enumFrom $ toEnum 0

lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m M.! s

replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ M.insert s v m

data ArmorSlot
  = Gloves
  | Head
  | Chest
  | Legs
  | Boots
  | Shoulders
  | Belt
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary ArmorSlot

data WeaponSlot
  = LeftHand
  | RightHand
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary WeaponSlot

data EquippableSlot
  = Body ArmorSlot
  | SingleHand
  | BothHands
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary EquippableSlot

type EquipmentSlot = Either ArmorSlot WeaponSlot

newtype Equipment =
  MkEquipment (Map EquipmentSlot UUID)
  deriving (Show, Read, Eq, Generic)

instance Binary Equipment

emptyEquipment :: Equipment
emptyEquipment = MkEquipment M.empty

equip :: EquipmentSlot -> UUID -> Equipment -> Equipment
equip slot uuid (MkEquipment eq) = MkEquipment $ M.insert slot uuid eq

unequip :: EquipmentSlot -> Equipment -> (Maybe UUID, Equipment)
unequip slot (MkEquipment eq) =
  (M.lookup slot eq, MkEquipment $ M.delete slot eq)

getEquippedInSlot :: EquipmentSlot -> Equipment -> Maybe UUID
getEquippedInSlot slot (MkEquipment eq) = M.lookup slot eq

getAllEquippedItems :: Equipment -> [UUID]
getAllEquippedItems (MkEquipment eq) = M.elems eq

data ItemType
  = Armor
  | Weapon
  | Potion
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary ItemType

data AIType
  = SimpleMeleeCreature
  | SimpleRangedCreature
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary AIType

-- {-# INLINABLE isHand #-}
-- isHand :: EquipmentSlot -> Bool
-- isHand LeftHand = True
-- isHand RightHand = True
-- isHand _ = False
--
-- {-# INLINABLE removeEquipment #-}
-- removeEquipment :: EquipmentSlot -> Equipment -> Maybe (Equipment, UUID)
-- removeEquipment slot (MkEquipment eqp) =
--   M.lookup slot eqp >>= \uuid -> Just (MkEquipment $ M.delete slot eqp, uuid)
--
-- {-# INLINABLE replaceEquipment #-}
-- replaceEquipment :: EquipmentSlot
--                  -> UUID
--                  -> Equipment
--                  -> (Maybe UUID, Equipment)
-- replaceEquipment slot eqid (MkEquipment eqp) =
--   let newEqp = MkEquipment $ M.insert slot eqid eqp
--   in case M.lookup slot eqp of
--        Just oldEqID -> (Just oldEqID, newEqp)
--        Nothing -> (Nothing, newEqp)
------------
-- CONSTS --
------------
mapWidthDOUBLE :: Double
mapWidthDOUBLE = 100

mapHeightDOUBLE :: Double
mapHeightDOUBLE = 25

mapWidthINT :: Int
mapWidthINT = round mapWidthDOUBLE

mapHeightINT :: Int
mapHeightINT = round mapHeightDOUBLE
