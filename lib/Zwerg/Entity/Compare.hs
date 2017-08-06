module Zwerg.Entity.Compare (getPrimaryOccupant) where

import Zwerg.Prelude

import Zwerg.Component
import Zwerg.Debug
import Zwerg.Util

compareEntitiesForVisibility :: UUID -> UUID -> MonadCompRead Ordering
compareEntitiesForVisibility uuid1 uuid2 = do
    (etype1,etype2) <- entityType <~~!> (uuid1,uuid2)
    if etype1 /= etype2
       then return (compare etype1 etype2)
       else compareEntitiesForVisibility' uuid1 uuid2 etype1

compareEntitiesForVisibility' :: UUID -> UUID -> EntityType -> MonadCompRead Ordering
compareEntitiesForVisibility' uuid1 uuid2 Item = do
    (itype1,itype2) <- itemType <~~!> (uuid1,uuid2)
    if itype1 /= itype2
       then return (compare itype1 itype2)
       else return GT

compareEntitiesForVisibility' _ _ Tile = do
    debug "should never be comparing the visibility preference for two Tiles."
    return GT

compareEntitiesForVisibility' _ _ Enemy = do
    debug "should never be comparing the visibility preference for two Enemies."
    return GT

compareEntitiesForVisibility' _ _ _ = return GT

getPrimaryOccupant :: UUID -> MonadCompRead UUID
getPrimaryOccupant occupiedUUID = do
    unwrap <$> occupants <~> occupiedUUID >>= \case
        [] -> return occupiedUUID
        occs -> head <$> (sortByM compareEntitiesForVisibility occs)
