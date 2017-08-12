module Zwerg.Prelude.Primitives
  ( Direction(..)
  , cardinalDirections
  , diagonalDirections
  , allDirections
  , TargetType(..)
  , Color(..)
  , CellColor(..)
  , Glyph(..)
  , EntityType(..)
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
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

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

--TODO: map this onto 256 color?
  --TODO: fallback 8 bit color?
data Color
  = Green0 | Green1 | Green2 | Green3
  | Blue0  | Blue1  | Blue2  | Blue3
  | Red0   | Red1   | Red2   | Red3
  | Black0 | Black1 | Black2 | Black3
  | White0 | White1 | White2 | White3
  deriving (Generic)
instance Binary Color

-- type ColoredText = [(Color, Text)]

data CellColor = CellColor
  { _cellColorForeground :: Color
  , _cellColorBackground :: Color
  } deriving (Generic)
instance Binary CellColor

data Glyph = Glyph
  { _glyphChar    :: Char
  , _glyphVisible :: CellColor
  , _glyphFogged  :: Maybe CellColor
  } deriving (Generic)
instance Binary Glyph

instance ZDefault Glyph where
    zDefault = Glyph 'X' (CellColor Red0 White0) Nothing

data EntityType
  = Player
  | Enemy
  | Item
  | Container
  | Tile
  | Level
  deriving (Eq, Ord, Show, Generic)
instance Binary EntityType

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
mapWidthDOUBLE = 100

mapHeightDOUBLE :: Double
mapHeightDOUBLE = 25

mapWidthINT :: Int
mapWidthINT = round mapWidthDOUBLE

mapHeightINT :: Int
mapHeightINT = round mapHeightDOUBLE
