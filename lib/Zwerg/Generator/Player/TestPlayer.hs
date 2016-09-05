module Zwerg.Generator.Player.TestPlayer where

import Zwerg.Generator

import Zwerg.Component.All

import Control.Lens (use)

testPlayerGenerator :: Generator ()
testPlayerGenerator = do
    pid <- use targetUUID
    let addPlayerComp = addComp pid

    addPlayerComp name "Bob"
    addPlayerComp glyph $ mkGlyph '@' $ mkColor 255 255 255
    addPlayerComp hp $ mkHP (10,10)
    addPlayerComp entityType Player
    addPlayerComp equipment emptyEquipment
    addPlayerComp stats zeroStats

