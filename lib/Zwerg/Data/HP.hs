module Zwerg.Data.HP
  ( HP
  , adjustHP
  , adjustMaxHP
  , fullHeal
  ) where

import Zwerg.Prelude

newtype HP = MkHP (Int, Int)
  deriving (Show, Eq, Ord, Generic)

instance Binary HP

instance ZConstructable HP (Int, Int) where
  zConstruct (curHP, maxHP) =
    if | curHP >= 0 && curHP <= maxHP && maxHP > 0 ->
         return $ MkHP (curHP, maxHP)
       | otherwise ->
         $(throw) EngineFatal "Attempted to create an invalid HP object"

instance ZWrapped HP (Int, Int) where
  unwrap (MkHP hp) = hp

adjustHP :: (Int -> Int) -> HP -> HP
adjustHP f (MkHP (curHP, maxHP))
  | newHP < 0 = MkHP (0, maxHP)
  | newHP > maxHP = MkHP (maxHP, maxHP)
  | otherwise = MkHP (newHP, maxHP)
  where
    newHP = f curHP

adjustMaxHP :: (Int -> Int) -> HP -> HP
adjustMaxHP f (MkHP (curHP, maxHP))
  | newMaxHP < 0 = MkHP (1, 1)
  | curHP > newMaxHP = MkHP (newMaxHP, newMaxHP)
  | otherwise = MkHP (curHP, newMaxHP)
  where newMaxHP = f maxHP

fullHeal :: HP -> HP
fullHeal (MkHP (_, maxHP)) = MkHP (maxHP, maxHP)
