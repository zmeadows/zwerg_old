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
  deriving stock Generic
  deriving anyclass Binary

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
  deriving stock Generic
  deriving anyclass Binary

data EntityType
    = Player
    | Enemy
    | Item
    | Container
    | Tile
    | Level
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass Binary

isTypicallyStationary :: EntityType -> Bool
isTypicallyStationary Player = False
isTypicallyStationary Enemy = False
isTypicallyStationary _ = True

instance ZDefault EntityType where
    zDefault = Enemy

data TileType = Floor | Wall | Door | Void
    deriving stock Generic
    deriving anyclass Binary

data Stat = STR | DEX | INT | CHA | CON | WIS
    deriving stock (Show, Eq, Ord, Enum, Generic)
    deriving anyclass Binary

newtype Stats = MkStats (Map Stat Int)
    deriving stock Generic
    deriving anyclass Binary

instance ZDefault Stats where
    zDefault = MkStats $ M.fromList $ fmap (, 0) $ enumFrom $ toEnum 0

lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m M.! s

replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ M.insert s v m

-- TODO: modifyStat (Int -> Int)
-- TODO: maximum stat == 100?

data ItemType = Weapon | Armor | Potion | Scroll | Gold
    deriving stock (Eq, Ord, Generic)
    deriving anyclass Binary

instance ZDefault ItemType where
    zDefault = Weapon

data AIType = SimpleMeleeCreature | SimpleRangedCreature
    deriving stock Generic
    deriving anyclass Binary

instance ZDefault AIType where
    zDefault = SimpleMeleeCreature

mapWidthDOUBLE :: Double
mapWidthDOUBLE = 125

mapHeightDOUBLE :: Double
mapHeightDOUBLE = 35

mapWidthINT :: Int
mapWidthINT = round mapWidthDOUBLE

mapHeightINT :: Int
mapHeightINT = round mapHeightDOUBLE
