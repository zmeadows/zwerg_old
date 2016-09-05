module Zwerg.Component.Equipment where

import Zwerg.Component.UUID (UUID)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data EquipmentSlot =
      Gloves
    | LeftHand
    | RightHand
    | Head
    | Chest
    | Legs
    | Boots
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance Hashable EquipmentSlot

newtype Equipment = MkEquipment (HashMap EquipmentSlot UUID)
    deriving (Show, Read, Eq)

{-# INLINABLE emptyEquipment #-}
emptyEquipment :: Equipment
emptyEquipment = MkEquipment HM.empty

{-# INLINABLE isHand #-}
isHand :: EquipmentSlot -> Bool
isHand LeftHand = True
isHand RightHand = True
isHand _ = False

{-# INLINABLE removeEquipment #-}
removeEquipment :: EquipmentSlot -> Equipment -> Maybe (Equipment, UUID)
removeEquipment slot (MkEquipment eqp) =
    HM.lookup slot eqp >>= \uuid -> Just (MkEquipment $ HM.delete slot eqp, uuid)

{-# INLINABLE replaceEquipment #-}
replaceEquipment :: EquipmentSlot -> UUID -> Equipment -> (Maybe UUID, Equipment)
replaceEquipment slot eqid (MkEquipment eqp) =
    let newEqp = MkEquipment $ HM.insert slot eqid eqp in
        case HM.lookup slot eqp of
          Just oldEqID -> (Just oldEqID, newEqp)
          Nothing -> (Nothing, newEqp)
