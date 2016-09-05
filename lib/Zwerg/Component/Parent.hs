module Zwerg.Component.Parent where

import Zwerg.Component.UUID (UUID)

data Parent = Alive UUID | Dead | NoParent
    deriving (Show, Read, Eq)
