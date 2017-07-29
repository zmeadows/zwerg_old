module Zwerg.Generator.Item.Weapon (sword) where

import Zwerg.Generator

sword :: Generator
sword = do
    swordUUID <- popUUID
    addComp swordUUID name "Iron Short Sword"
    addComp swordUUID description "A simple sword with short iron blade"
    addComp swordUUID glyph $ Glyph '/' White3 Black1 Nothing Nothing
    addComp swordUUID entityType Item
    addComp swordUUID itemType Weapon
    addComp swordUUID damageChain
      [ DamageData SingleTarget Slash $ Uniform 1 2
      , DamageData SingleTarget Pierce $ Uniform 1 2
      ]
    addComp swordUUID blocksPassage False
    addComp swordUUID blocksVision False
    addComp swordUUID slot $ SingleHand RightHand
    return swordUUID
