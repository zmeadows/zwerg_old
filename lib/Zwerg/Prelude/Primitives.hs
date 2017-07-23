module Zwerg.Prelude.Primitives
  ( UUID
  , playerUUID
  , Direction(..)
  , TargetType(..)
  , Color(..)
  , Glyph(..)
  , EntityType(..)
  , Parent(..)
  , TileType(..)
  , Stat(..)
  , Stats(..)
  , zeroStats
  , lookupStat
  , replaceStat
  , AIType(..)
  , ItemType(..)
  , mapWidthDOUBLE
  , mapWidthINT
  , mapHeightDOUBLE
  , mapHeightINT
  ) where

import Protolude
import Zwerg.Prelude.Class
import Zwerg.Data.ZError

-- import Control.Monad.Random.Class as EXPORTED
import Data.Binary as EXPORTED (Binary)
-- import Data.Monoid as EXPORTED ((<>))
-- import Data.Text as EXPORTED (Text, pack, unpack)
-- import Data.Traversable as EXPORTED (forM)
-- import GHC.Generics as EXPORTED (Generic)
-- import Lens.Micro.Internal as EXPORTED
--        (At(..), Ixed(..), Index, IxValue)
-- import Lens.Micro.Platform as EXPORTED
--        (makeClassy, makeLenses, makeFields, (%=), (^.), (.=), over, use,
--         view, to, set, Lens', (<&>))
--
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

newtype UUID = MkUUID Int
  deriving (Show, Read, Eq, Bounded, Enum, Num, Ord, Generic)

instance Binary UUID

instance ZWrapped UUID Int where
  unwrap (MkUUID uuid) = uuid

instance ZConstructable UUID Int where
  zConstruct x =
    if | x >= 0 -> return $ MkUUID x
       | otherwise -> $(throw) EngineFatal "Attempted to construct UUID < 0"

playerUUID :: UUID
playerUUID = MkUUID 0

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
  | Line Direction Int
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


data Parent = Alive UUID | Dead
  deriving (Show, Read, Eq, Generic)

instance Binary Parent

data TileType = Floor | Wall | Door | Void
  deriving (Show, Read, Eq, Generic)

instance Binary TileType

data Stat = STR | DEX | INT | CHA | CON | WIS
  deriving (Read, Show, Eq, Ord, Enum, Generic)

instance Binary Stat

newtype Stats = MkStats (Map Stat Int)
  deriving (Show, Read, Eq, Generic)

instance Binary Stats

zeroStats :: Stats
zeroStats = MkStats $ M.fromList $ fmap (, 0) $ enumFrom $ toEnum 0

lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m M.! s

replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ M.insert s v m

-- TODO: modifyStat (Int -> Int)

data ItemType = Armor | Weapon | Potion | Scroll
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary ItemType

data AIType = SimpleMeleeCreature | SimpleRangedCreature
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary AIType

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
