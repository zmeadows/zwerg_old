module Zwerg.Data.Equipment
  ( Equipment,
    EquipmentSlot(..),
    HandSlot(..),
    emptyEquipment,
    equip,
    unequip,
    getAllEquippedItems,
    getEquippedInSlot
  ) where

import Zwerg.Prelude
import Zwerg.Data.UUIDSet

import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as M (empty, insert, delete, lookup, elems)

data ArmorSlot = Gloves | Head | Chest | Legs | Boots | Shoulders | Belt
  deriving (Show, Eq, Ord, Enum, Generic)
instance Binary ArmorSlot

data HandSlot = LeftHand | RightHand
  deriving (Show, Eq, Ord, Enum, Generic)
instance Binary HandSlot

data EquipmentSlot = Body ArmorSlot | SingleHand HandSlot | BothHands
  deriving (Show, Eq, Ord, Generic)
instance Binary EquipmentSlot

newtype Equipment = MkEquipment (Map EquipmentSlot UUID)
  deriving (Show, Eq, Generic)
instance Binary Equipment

{-# INLINABLE emptyEquipment #-}
emptyEquipment :: Equipment
emptyEquipment = MkEquipment M.empty

{-# INLINABLE equip #-}
equip :: EquipmentSlot -> UUID -> Equipment -> ([UUID], Equipment)
equip slot uuid eq =
    let (ueis, MkEquipment eq') = unequip slot eq
    in (ueis, MkEquipment $ M.insert slot uuid eq')

{-# INLINABLE unequip #-}
unequip :: EquipmentSlot -> Equipment -> ([UUID], Equipment)
unequip (SingleHand LeftHand) eq  = unequipSlots [SingleHand LeftHand, BothHands] eq
unequip (SingleHand RightHand) eq = unequipSlots [SingleHand RightHand, BothHands] eq
unequip BothHands eq            = unequipSlots [SingleHand LeftHand, SingleHand RightHand, BothHands] eq
unequip slot eq                 = unequipSlots [slot] eq

{-# INLINABLE unequipSlots #-}
unequipSlots :: [EquipmentSlot] -> Equipment -> ([UUID], Equipment)
unequipSlots slots eq = unequipSlots' slots eq []

{-# INLINABLE unequipSlots' #-}
unequipSlots' :: [EquipmentSlot] -> Equipment -> [UUID] -> ([UUID], Equipment)
unequipSlots' [] eq uis = (uis, eq)
unequipSlots' (s:ss) (MkEquipment eq) uis =
  let eq' = MkEquipment $ M.delete s eq
  in case M.lookup s eq of
       Just uuid -> unequipSlots' ss eq' (uuid:uis)
       Nothing -> ([], eq')

{-# INLINABLE getEquippedInSlot #-}
getEquippedInSlot :: EquipmentSlot -> Equipment -> [UUID]

getEquippedInSlot (SingleHand LeftHand) (MkEquipment eqMap)  =
  catMaybes $ map (`M.lookup` eqMap) [SingleHand LeftHand, BothHands]

getEquippedInSlot (SingleHand RightHand) (MkEquipment eqMap) =
  catMaybes $ map (`M.lookup` eqMap) [SingleHand RightHand, BothHands]

getEquippedInSlot (BothHands) (MkEquipment eqMap)          =
  catMaybes $ map (`M.lookup` eqMap) [SingleHand LeftHand, SingleHand RightHand, BothHands]

getEquippedInSlot slot (MkEquipment eqMap) = catMaybes [M.lookup slot eqMap]

{-# INLINABLE getAllEquippedItems #-}
getAllEquippedItems :: Equipment -> UUIDSet
getAllEquippedItems (MkEquipment eq) = zFromList $ M.elems eq
