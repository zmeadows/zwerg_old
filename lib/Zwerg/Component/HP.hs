module Zwerg.Component.HP (
    HP,
    adjustHP,
    adjustMaxHP,
    fullHeal
    ) where

import Zwerg.Prelude
import Zwerg.Class
import Zwerg.Data.Error

import Data.Binary
import GHC.Generics (Generic)

newtype HP = MkHP (Int,Int)
    deriving (Show, Read, Eq, Ord, Generic)

instance Binary HP

instance ZConstructable HP (Int,Int) where
  zConstruct (curHP, maxHP) = if
    | curHP >= 0 && curHP <= maxHP && maxHP > 0 -> return $ MkHP (curHP,maxHP)
    | otherwise -> throwError $ ZError __FILE__ __LINE__ Fatal
                                "Attempted to create an invalid HP object"

instance ZWrapped HP (Int,Int) where
  unwrap (MkHP hp) = hp

{-# INLINABLE adjustHP #-}
adjustHP :: (Int -> Int) -> HP -> HP
adjustHP f (MkHP (curHP,maxHP))
    | newHP < 0 = MkHP (0,maxHP)
    | newHP > maxHP = MkHP (maxHP, maxHP)
    | otherwise = MkHP (newHP , maxHP)
  where newHP = f curHP

{-# INLINABLE adjustMaxHP #-}
adjustMaxHP :: (Int -> Int) -> HP -> HP
adjustMaxHP f (MkHP (curHP,maxHP))
    | newMaxHP < 0 = MkHP (1,1)
    | curHP > newMaxHP = MkHP (newMaxHP, newMaxHP)
    | otherwise = MkHP (curHP, newMaxHP)
  where newMaxHP = f maxHP

{-# INLINABLE fullHeal #-}
fullHeal :: HP -> HP
fullHeal (MkHP (_,maxHP)) = MkHP (maxHP,maxHP)
