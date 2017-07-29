module Zwerg.Generator.Player.TestPlayer where

import Zwerg.Generator
import Zwerg.Generator.Item.Weapon

testPlayerGenerator :: UUID -> Generator' ()
testPlayerGenerator startLevelUUID = do
    let addPlayerComp = addComp playerUUID
    addPlayerComp name "Bob"
    addPlayerComp description "It's you."
    addPlayerComp level startLevelUUID
    addPlayerComp glyph $ Glyph '@' Red0 Red0 Nothing Nothing
    zConstruct (7, 10) >>= addPlayerComp hp
    addPlayerComp entityType Player
    addPlayerComp equipment emptyEquipment

    addPlayerComp stats zeroStats
    assignUniformRandomStat playerUUID STR (1, 100)
    assignUniformRandomStat playerUUID DEX (2, 100)
    assignUniformRandomStat playerUUID INT (1, 100)
    assignUniformRandomStat playerUUID CHA (1, 100)
    assignUniformRandomStat playerUUID CON (1, 100)
    assignUniformRandomStat playerUUID WIS (1, 100)

    addPlayerComp viewRange 7.0
    addPlayerComp ticks 50
    addPlayerComp blocksPassage True
    addPlayerComp blocksVision False
    putOnRandomEmptyTile startLevelUUID playerUUID

    -- TODO: use tileOn for equipped items as well
    swordUUID <- sword
    equipItem swordUUID playerUUID

    anotherSword <- sword
    addComp playerUUID inventory zEmpty
    modComp playerUUID inventory (zAdd anotherSword)
