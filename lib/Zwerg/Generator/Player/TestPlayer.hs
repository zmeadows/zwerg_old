module Zwerg.Generator.Player.TestPlayer where

import Zwerg.Generator
import Zwerg.Generator.Item.Weapon
import Zwerg.Util

testPlayerGenerator :: UUID -> Generator ()
testPlayerGenerator startLevelUUID =
  MkGenerator $ do
    let addPlayerComp = addComp playerUUID
    addPlayerComp name "Bob"
    addPlayerComp level startLevelUUID
    addPlayerComp glyph $ Glyph '@' Red0 Red0 Nothing Nothing
    zConstruct (10, 10) >>= addPlayerComp hp
    addPlayerComp entityType Player
    addPlayerComp equipment emptyEquipment
    addPlayerComp stats zeroStats
    addPlayerComp viewRange 5.0
    addPlayerComp ticks 50
    addPlayerComp blocksPassage True
    playerTileUUID <- getRandomEmptyTile startLevelUUID
    playerTileUUID' <-
      fromJustErrM playerTileUUID $
      ZError
        __FILE__
        __LINE__
        Fatal
        "Could not find an open tile to place Player"
    demandComp position playerTileUUID' >>= addPlayerComp position
    addOccupant playerUUID playerTileUUID'
    swordUUID <- generate sword
    return ()
