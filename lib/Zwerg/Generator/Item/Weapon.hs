module Zwerg.Generator.Item.Weapon (sword) where

import Zwerg.Generator
import Zwerg.Generator.Default
import Zwerg.Generator.Verify

sword :: Generator
sword = do
    swordUUID <- generateSkeleton Item
    let (<@-) :: Component a -> a -> Generator' ()
        (<@-) = addComp swordUUID

    name        <@- "Iron Short Sword"
    description <@- "A simple sword with short iron blade"
    glyph       <@- (Glyph '/' $ CellColor antiquewhite Nothing)
    itemType    <@- Weapon
    damageChain <@- [ DamageData SingleTarget Slash $ Uniform 1 2 , DamageData SingleTarget Pierce $ Uniform 1 2 ]
    slot        <@- SingleHand RightHand

    verifyAndReturn swordUUID
