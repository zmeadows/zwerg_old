module Zwerg.Generator.Item.Weapon
  ( sword
  ) where

import Zwerg.Generator

sword :: Generator
sword = MkGenerator $ do
    swordUUID <- popUUID
    addComp swordUUID name "Short Sword"
    addComp swordUUID glyph $ Glyph '/' Black1 Black3 Nothing Nothing
    addComp swordUUID entityType Item
    addComp swordUUID itemType Weapon
    addComp swordUUID damageChain [DamageData SingleTarget Slash (Uniform 1 6)]
    addComp swordUUID blocksPassage False
    addComp swordUUID blocksVision False
    addComp swordUUID slot $ SingleHand RightHand
    return swordUUID
