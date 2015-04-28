{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Zwerg.Port where

import Zwerg.Types
import Zwerg.Entity
import Zwerg.Entity.Player
import Zwerg.Layer.Simple

import Data.Label

import Data.Text (Text)

data Port = MainMenu [Text] Int
          | OverWorld deriving (Show, Read, Eq)

mainMenu :: Port
mainMenu = MainMenu ["New Game", "Options", "About", "Quit"] 0

processMove :: Direction -> System Port
processMove dir = movePlayer dir >> return OverWorld

processInput :: Port -> (KeyMod, KeyCode) -> System Port
processInput OverWorld (None, Letter 'k') = processMove North

processInput OverWorld (None, Letter 'h') = processMove West

processInput OverWorld (None, Letter 'j') = processMove South

processInput OverWorld (None, Letter 'l') = processMove East

processInput (MainMenu entries focus) (None, Letter 'j') =
     return $ MainMenu entries $ mod (focus + 1) $ length entries

processInput (MainMenu entries focus) (None, Letter 'k') =
     return $ MainMenu entries $ mod (focus - 1) $ length entries

processInput (MainMenu entries 0) (None, Return) = do
    layerUUID <- makeSimpleLayer 20 20
    makePlayer "Bob" (5,5) 10 layerUUID
    return OverWorld


processInput port _ = return port

