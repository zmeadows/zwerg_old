module Zwerg.Component.Stats where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Stat = STR | DEX | INT | CHA | CON | WIS
    deriving (Read, Show, Eq, Ord, Enum, Generic)

instance Hashable Stat

newtype StatMod = MkStatMod (Stat,Int)
    deriving (Read, Show, Eq, Ord)

newtype Stats = MkStats (HashMap Stat Int)
    deriving (Show, Read, Eq)

{-# INLINABLE zeroStats #-}
zeroStats :: Stats
zeroStats = MkStats $ HM.fromList $ map (,0) $ enumFrom $ toEnum 0

{-# INLINABLE lookupStat #-}
lookupStat :: Stat -> Stats -> Int
lookupStat s (MkStats m) = m HM.! s

{-# INLINABLE replaceStat #-}
replaceStat :: Stat -> Int -> Stats -> Stats
replaceStat s v (MkStats m) = MkStats $ HM.insert s v m
