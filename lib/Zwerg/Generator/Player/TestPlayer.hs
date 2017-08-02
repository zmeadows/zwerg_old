module Zwerg.Generator.Player.TestPlayer where

import Zwerg.Generator
import Zwerg.Generator.Default
import Zwerg.Generator.Item.Weapon

testPlayerGenerator :: UUID -> Generator' ()
testPlayerGenerator startLevelUUID = do
    generatePlayerSkeleton
    let (<@-) :: Component a -> a -> Generator' ()
        (<@-) = addComp playerUUID

    newHP <- zConstruct (100, 100)

    name        <@- "Bob"
    description <@- "It's you."
    level       <@- startLevelUUID
    entityType  <@- Player
    viewRange   <@- 7.0
    ticks       <@- 50
    hp          <@- newHP

    assignUniformRandomStat playerUUID STR (1, 100)
    assignUniformRandomStat playerUUID DEX (2, 100)
    assignUniformRandomStat playerUUID INT (1, 100)
    assignUniformRandomStat playerUUID CHA (1, 100)
    assignUniformRandomStat playerUUID CON (1, 100)
    assignUniformRandomStat playerUUID WIS (1, 100)

    putOnRandomEmptyTile startLevelUUID playerUUID

    generateAndHoldN 3 sword playerUUID

