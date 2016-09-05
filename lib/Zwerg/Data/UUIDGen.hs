module Zwerg.Data.UUIDGen (
    UUIDGen,
    HasUUIDGen(..),
    initUUIDGen,
    popUUID,
    pushUUID,
    getNewUUID
    ) where

import Zwerg.Component.UUID

import Prelude hiding (ceiling)
import Control.Lens ((^.), (.=), over, use, to, makeLenses, Lens')

import Control.Monad.State (MonadState)

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (
    empty,
    insert,
    delete,
    lookupGE
    )


data UUIDGen = UUIDGen
    { _graveyard :: IntSet
    , _ceiling :: Int
    } deriving (Show, Read, Eq)
makeLenses ''UUIDGen

class HasUUIDGen s where
    uuidGen :: Lens' s UUIDGen

{-# INLINABLE initUUIDGen #-}
initUUIDGen :: UUIDGen
initUUIDGen = UUIDGen IS.empty 0

{-# INLINABLE popUUID #-}
popUUID :: UUIDGen -> (UUID, UUIDGen)
popUUID ug =
    let dugupUUID = IS.lookupGE 0 $ ug ^. graveyard
    in case dugupUUID of
          Just i -> (mkUUID i, over graveyard (IS.delete i) ug)
          Nothing -> (mkUUID $ ug ^. ceiling, over ceiling (+1) ug)

{-# INLINABLE pushUUID #-}
pushUUID :: UUID -> UUIDGen -> UUIDGen
pushUUID uuid = over graveyard (IS.insert $ unUUID uuid)

{-# INLINABLE getNewUUID #-}
getNewUUID :: (HasUUIDGen s, MonadState s m) => m UUID
getNewUUID = do
    (nextUUID, newUUIDGen) <- use (uuidGen . to popUUID)
    uuidGen .= newUUIDGen
    return nextUUID
