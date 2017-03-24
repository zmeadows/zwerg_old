module Zwerg.Component.UUID (
    UUID,
    playerUUID,
    UUIDGen,
    HasUUIDGen(..),
    initUUIDGen,
    popUUID,
    pushUUID,
    getNewUUID
    ) where

import Zwerg.Prelude hiding (ceiling)
import Zwerg.Class

import GHC.Generics (Generic)
import Data.Binary
import System.Random
import Control.Lens ((^.), (.=), over, use, to, makeLenses, Lens')
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS (insert, delete, lookupGT, singleton)

newtype UUID = MkUUID Int
    deriving (Show, Read, Eq, Num, Ord, Generic, Random)

instance Binary UUID

instance ZWrapped UUID Int where
  unwrap (MkUUID uuid) = uuid

playerUUID :: UUID
playerUUID = MkUUID 0

data UUIDGen = UUIDGen
    { _graveyard :: IntSet
    , _ceiling   :: Int
    } deriving (Show, Read, Eq)
makeLenses ''UUIDGen

class HasUUIDGen s where
    uuidGen :: Lens' s UUIDGen

{-# INLINABLE initUUIDGen #-}
initUUIDGen :: UUIDGen
initUUIDGen = UUIDGen (IS.singleton $ unwrap playerUUID) $ unwrap playerUUID + 1

{-# INLINABLE popUUID #-}
popUUID :: UUIDGen -> (UUID, UUIDGen)
popUUID ug =
    let dugupUUID = IS.lookupGT 0 $ ug ^. graveyard
    in case dugupUUID of
          Just i -> (MkUUID i, over graveyard (IS.delete i) ug)
          Nothing -> (MkUUID $ ug ^. ceiling, over ceiling (+1) ug)

{-# INLINABLE pushUUID #-}
pushUUID :: UUID -> UUIDGen -> UUIDGen
pushUUID (MkUUID uuid) = over graveyard (IS.insert uuid)

{-# INLINABLE getNewUUID #-}
getNewUUID :: (HasUUIDGen s, MonadState s m) => m UUID
getNewUUID = do
    (nextUUID, newUUIDGen) <- use (uuidGen . to popUUID)
    uuidGen .= newUUIDGen
    return nextUUID
