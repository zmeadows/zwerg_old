module Zwerg.Prelude.Primitives
  ( Direction(..)
  , cardinalDirections
  , diagonalDirections
  , allDirections
  , TargetType(..)
  , EntityType(..)
  , isTypicallyStationary
  , TileType(..)
  , Stat(..)
  , Stats(..)
  , lookupStat
  , replaceStat
  , AIType(..)
  , ItemType(..)
  , mapWidthDOUBLE
  , mapWidthINT
  , mapHeightDOUBLE
  , mapHeightINT
  ) where

import Prelude

import Zwerg.Prelude.Class

import Data.Binary as EXPORTED (Binary)
import GHC.Generics (Generic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data Direction
  = North
  | South
  | East
  | West
  | NorthWest
  | NorthEast
  | SouthWest
  | SouthEast
  deriving (Generic)
instance Binary Direction

cardinalDirections :: [Direction]
cardinalDirections = [North,South,East,West]

diagonalDirections :: [Direction]
diagonalDirections = [NorthWest,SouthWest,NorthEast,SouthEast]

allDirections :: [Direction]
allDirections = cardinalDirections ++ diagonalDirections

data TargetType
  = SingleTarget
  | AOE Double
  | Line Direction Int
  deriving (Generic)
instance Binary TargetType

data EntityType
  = Player
  | Enemy
  | Item
  | Container
  | Tile
  | Level
  deriving (Eq, Ord, Show, Generic)
instance Binary EntityType

isTypicallyStationary :: EntityType -> Bool
isTypicallyStationary Player = False
isTypicallyStationary Enemy = False
isTypicallyStationary _ = True

instance ZDefault EntityType where
    zDefault = Enemy

data TileType = Floor | Wall | Door | Void
  deriving (Generic)
instance Binary TileType

data Stat = STR | DEX | INT | CHA | CON | WIS
    deriving (Show, Eq, Ord, Enum, Generic)
instance Binary Stat

newtype Stats = MkStats (Map Stat Int)
  deriving (Generic)
instance Binary Stats

instance ZDefault Stats where
    zDefault = MkStats $ M.fromList $ fmap (, 0) $ enumFrom $ toEnum 0

lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m M.! s

replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ M.insert s v m

-- TODO: modifyStat (Int -> Int)
-- TODO: maximum stat == 100?

data ItemType = Weapon | Armor | Potion | Scroll | Gold
  deriving (Eq, Ord, Generic)
instance Binary ItemType

instance ZDefault ItemType where
    zDefault = Weapon

data AIType = SimpleMeleeCreature | SimpleRangedCreature
  deriving (Generic)
instance Binary AIType

instance ZDefault AIType where
    zDefault = SimpleMeleeCreature

------------
-- CONSTS --
------------
mapWidthDOUBLE :: Double
mapWidthDOUBLE = 125

mapHeightDOUBLE :: Double
mapHeightDOUBLE = 35

mapWidthINT :: Int
mapWidthINT = round mapWidthDOUBLE

mapHeightINT :: Int
mapHeightINT = round mapHeightDOUBLE
