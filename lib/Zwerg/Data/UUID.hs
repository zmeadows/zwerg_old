module Zwerg.Data.UUID
  ( UUID
  , worldUUID
  , playerUUID
  , incUUID
  , Parent(..)
  ) where

import Prelude

import Zwerg.Prelude.Class

import Data.Binary (Binary)
import GHC.Generics (Generic)

newtype UUID = MkUUID Int
  deriving (Show, Eq, Bounded, Enum, Ord, Generic, Binary)

instance ZDefault UUID where
    zDefault = MkUUID (-1)

instance ZWrapped UUID Int where
  unwrap (MkUUID uuid) = uuid
  wrap i = if i >= 0 then Just (MkUUID i) else Nothing

playerUUID, worldUUID :: UUID
worldUUID = MkUUID 0
playerUUID = MkUUID 1

incUUID :: UUID -> UUID
incUUID (MkUUID uuid) = MkUUID $ uuid + 1

data Parent = Alive UUID | Dead | NoParent
  deriving (Show, Eq, Generic)
instance Binary Parent
