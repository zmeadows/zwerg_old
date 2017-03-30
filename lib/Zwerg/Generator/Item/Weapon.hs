module Zwerg.Generator.Item.Weapon
  ( swordGenerator
  ) where

import Zwerg.Event
import Zwerg.Generator

swordGenerator :: Generator UUID
swordGenerator =
  MkGenerator $ do
    swordUUID <- getNewUUID
    addComp swordUUID name "Short Sword"
    addComp swordUUID glyph $ Glyph Normal '/' $ mkColor 150 150 150
    addComp swordUUID entityType Item
    addComp swordUUID damageChain [DamageData SingleTarget Slash (Uniform 1 5)]
    return swordUUID
