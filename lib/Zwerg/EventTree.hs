module Zwerg.EventTree (
    EventTree
    ) where

import Zwerg.Event

import Data.Tree (Tree)

newtype EventTree = MkEventTree (Tree Event)
    deriving (Show, Read, Eq)

