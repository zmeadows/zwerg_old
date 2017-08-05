module Zwerg.Data.UUID
  ( UUID
  , worldUUID
  , playerUUID
  , incUUID
  , Parent(..)
  ) where

import Prelude

import Zwerg.Prelude.Class
import Zwerg.Data.ZError

import Data.Binary (Binary)
import GHC.Generics (Generic)

newtype UUID = MkUUID Int
  deriving (Show, Eq, Bounded, Enum, Ord, Generic, Binary)

instance ZWrapped UUID Int where
  unwrap (MkUUID uuid) = uuid
  wrap i = if i >= 0 then Just (MkUUID i) else Nothing

instance ZConstructable UUID Int where
  zConstruct x =
    if | x >= 0 -> return $ MkUUID x
       | otherwise -> $(throw) EngineFatal "Attempted to construct UUID < 0"

playerUUID, worldUUID :: UUID
worldUUID = MkUUID 0
playerUUID = MkUUID 1

incUUID :: UUID -> UUID
incUUID (MkUUID uuid) = MkUUID $ uuid + 1

data Parent = Alive UUID | Dead | NoParent
  deriving (Show, Eq, Generic)
instance Binary Parent
