module Zwerg.Component.Parent where

import Zwerg.Prelude
import Zwerg.Component.UUID (UUID)

import GHC.Generics (Generic)
import Data.Binary

data Parent = Alive UUID | Dead | NoParent
    deriving (Show, Read, Eq, Generic)

instance Binary Parent
