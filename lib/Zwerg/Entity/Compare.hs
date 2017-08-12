module Zwerg.Entity.Compare (getPrimaryOccupant, getPrimaryNonEnemyOccupant) where

import Zwerg.Prelude

import Zwerg.Component
import Zwerg.Debug
import Zwerg.Util

import Control.Monad.Loops (firstM)

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
       --TODO: further if/then/else compare same ItemType
       then return (compare itype1 itype2)
       else return GT

compareEntitiesForVisibility' _ _ etype = do
    debug $ "should never be comparing the visibility preference for two entities of type: "
            <> show etype
    return GT

getPrimaryOccupant :: UUID -> MonadCompRead UUID
getPrimaryOccupant occupiedUUID =
    unwrap <$> occupants <~> occupiedUUID >>= \case
        [] -> return occupiedUUID
        occs -> head <$> sortByM compareEntitiesForVisibility occs

getPrimaryNonEnemyOccupant :: UUID -> MonadCompRead UUID
getPrimaryNonEnemyOccupant occupiedUUID =
    unwrap <$> occupants <~> occupiedUUID >>= \case
        [] -> return occupiedUUID
        occs -> do
            sortedOccs <- sortByM compareEntitiesForVisibility occs
            firstM (\uuid -> (/= Enemy) <$> entityType <~> uuid) sortedOccs >>= \case
                Nothing -> return occupiedUUID
                Just x -> return x
                
