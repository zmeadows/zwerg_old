module Zwerg.Generator.Item.Weapon (sword) where

import Zwerg.Generator
import Zwerg.Generator.Default

sword :: Generator
sword = do
    swordUUID <- popUUID
    generateSkeleton swordUUID Item
    addComp swordUUID name "Iron Short Sword"
    addComp swordUUID description "A simple sword with short iron blade"
    addComp swordUUID glyph $ Glyph '/' (CellColor White3 Black1) Nothing
    addComp swordUUID itemType Weapon
    addComp swordUUID damageChain
      [ DamageData SingleTarget Slash $ Uniform 1 2
      , DamageData SingleTarget Pierce $ Uniform 1 2
      ]
    addComp swordUUID slot $ SingleHand RightHand
    return swordUUID
