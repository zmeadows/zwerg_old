module Zwerg.Data.Equipment where

import Zwerg.Prelude

import Data.Map (Map)
import qualified Data.Map as M (empty, insert, delete, lookup, elems)

data ArmorSlot
  = Gloves
  | Head
  | Chest
  | Legs
  | Boots
  | Shoulders
  | Belt
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary ArmorSlot

data WeaponSlot
  = LeftHand
  | RightHand
  deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Binary WeaponSlot

data EquippableSlot
  = Body ArmorSlot
  | SingleHand
  | BothHands
  deriving (Show, Read, Eq, Ord, Generic)

instance Binary EquippableSlot

type EquipmentSlot = Either ArmorSlot WeaponSlot

newtype Equipment =
  MkEquipment (Map EquipmentSlot UUID)
  deriving (Show, Read, Eq, Generic)

instance Binary Equipment

emptyEquipment :: Equipment
emptyEquipment = MkEquipment M.empty

equip :: EquipmentSlot -> UUID -> Equipment -> Equipment
equip slot uuid (MkEquipment eq) = MkEquipment $ M.insert slot uuid eq

unequip :: EquipmentSlot -> Equipment -> (Maybe UUID, Equipment)
unequip slot (MkEquipment eq) =
  (M.lookup slot eq, MkEquipment $ M.delete slot eq)

getEquippedInSlot :: EquipmentSlot -> Equipment -> Maybe UUID
getEquippedInSlot slot (MkEquipment eq) = M.lookup slot eq

getAllEquippedItems :: Equipment -> [UUID]
getAllEquippedItems (MkEquipment eq) = M.elems eq

-- {-# INLINABLE isHand #-}
-- isHand :: EquipmentSlot -> Bool
-- isHand LeftHand = True
-- isHand RightHand = True
-- isHand _ = False
--
-- {-# INLINABLE removeEquipment #-}
-- removeEquipment :: EquipmentSlot -> Equipment -> Maybe (Equipment, UUID)
-- removeEquipment slot (MkEquipment eqp) =
--   M.lookup slot eqp >>= \uuid -> Just (MkEquipment $ M.delete slot eqp, uuid)
--
-- {-# INLINABLE replaceEquipment #-}
-- replaceEquipment :: EquipmentSlot
--                  -> UUID
--                  -> Equipment
--                  -> (Maybe UUID, Equipment)
-- replaceEquipment slot eqid (MkEquipment eqp) =
--   let newEqp = MkEquipment $ M.insert slot eqid eqp
--   in case M.lookup slot eqp of
--        Just oldEqID -> (Just oldEqID, newEqp)
--        Nothing -> (Nothing, newEqp)
