module Zwerg.Component.Equipment where

import Zwerg.Prelude
import Zwerg.Component.UUID (UUID)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import GHC.Generics (Generic)
import Data.Binary

data EquipmentSlot =
      Gloves
    | LeftHand
    | RightHand
    | Head
    | Chest
    | Legs
    | Boots
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary EquipmentSlot

newtype Equipment = MkEquipment (Map EquipmentSlot UUID)
    deriving (Show, Read, Eq, Generic)

instance Binary Equipment

{-# INLINABLE emptyEquipment #-}
emptyEquipment :: Equipment
emptyEquipment = MkEquipment M.empty

{-# INLINABLE isHand #-}
isHand :: EquipmentSlot -> Bool
isHand LeftHand = True
isHand RightHand = True
isHand _ = False

{-# INLINABLE removeEquipment #-}
removeEquipment :: EquipmentSlot -> Equipment -> Maybe (Equipment, UUID)
removeEquipment slot (MkEquipment eqp) =
    M.lookup slot eqp >>= \uuid -> Just (MkEquipment $ M.delete slot eqp, uuid)

{-# INLINABLE replaceEquipment #-}
replaceEquipment :: EquipmentSlot -> UUID -> Equipment -> (Maybe UUID, Equipment)
replaceEquipment slot eqid (MkEquipment eqp) =
    let newEqp = MkEquipment $ M.insert slot eqid eqp in
        case M.lookup slot eqp of
          Just oldEqID -> (Just oldEqID, newEqp)
          Nothing -> (Nothing, newEqp)
