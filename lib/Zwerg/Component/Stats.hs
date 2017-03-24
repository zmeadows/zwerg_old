module Zwerg.Component.Stats where

import Zwerg.Prelude

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Data.Binary

data Stat = STR | DEX | INT | CHA | CON | WIS
    deriving (Read, Show, Eq, Ord, Enum, Generic)

instance Binary Stat

newtype StatMod = MkStatMod (Stat,Int)
    deriving (Read, Show, Eq, Ord, Generic)

newtype Stats = MkStats (Map Stat Int)
    deriving (Show, Read, Eq, Generic)

instance Binary Stats

{-# INLINABLE zeroStats #-}
zeroStats :: Stats
zeroStats = MkStats $ M.fromList $ fmap (,0) $ enumFrom $ toEnum 0

{-# INLINABLE lookupStat #-}
lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m M.! s

{-# INLINABLE replaceStat #-}
replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ M.insert s v m
