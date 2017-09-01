module Zwerg.Generator.Player.TestPlayer where

import Zwerg.Generator
import Zwerg.Generator.Default
import Zwerg.Generator.Item.Weapon

testPlayer :: Generator
testPlayer =
    Generator testPlayerHatcher []
      +> assignUniformRandomStats (zip (enumFrom STR) (repeat (1,100)))
      +> generateAndEquip sword

testPlayerHatcher :: EntityHatcher
testPlayerHatcher = MkEntityHatcher $ do
    generatePlayerSkeleton
    let (<@-) :: Component a -> a -> MonadCompState ()
        (<@-) = addComp playerUUID

    name        <@- "Bob"
    description <@- "It's you."
    entityType  <@- Player
    viewRange   <@- 7
    ticks       <@- 50
    hp          <@- unsafeWrap (100,100)

    return playerUUID

    -- putOnRandomEmptyTile startLevelUUID playerUUID
    -- generateAndHoldN 3 sword playerUUID
