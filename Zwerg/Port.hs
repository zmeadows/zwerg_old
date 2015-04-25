{-# LANGUAGE TemplateHaskell #-}
module Zwerg.Port where

import Zwerg.Types
import Zwerg.Entity

import Data.Label

import Data.Text (Text)

fclabels [d|
  data Port = MainMenu [Text] Int
            | OverWorld deriving (Show, Read, Eq)
  |]


processMove :: Direction -> System Port
processMove dir = movePlayer dir >> return OverWorld

processInput :: Port -> (KeyMod, KeyCode) -> System Port
processInput OverWorld (None, Letter 'w') = processMove North

processInput OverWorld (None, Letter 'a') = processMove West

processInput OverWorld (None, Letter 's') = processMove South

processInput OverWorld (None, Letter 'd') = processMove East

processInput port _ = return port

