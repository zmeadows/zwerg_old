module Zwerg.Component.UUID
  ( UUIDGen
  , HasUUIDGen(..)
  , initUUIDGen
  , mkUUID
  , popUUID
  , pushUUID
  , getNewUUID
  ) where

import Zwerg.Prelude hiding (ceiling)

import Control.Lens ((^.), (.=), over, use, to, makeLenses, Lens')
import Data.Binary
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
       (insert, delete, lookupGT, singleton)
import GHC.Generics (Generic)
import System.Random

data UUIDGen = UUIDGen
  { _graveyard :: IntSet
  , _ceiling :: Int
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
       Nothing -> (MkUUID $ ug ^. ceiling, over ceiling (+ 1) ug)

{-# INLINABLE pushUUID #-}
pushUUID :: UUID -> UUIDGen -> UUIDGen
pushUUID (MkUUID uuid) = over graveyard (IS.insert uuid)

{-# INLINABLE getNewUUID #-}
getNewUUID
  :: (HasUUIDGen s, MonadState s m)
  => m UUID
getNewUUID = do
  (nextUUID, newUUIDGen) <- use (uuidGen . to popUUID)
  uuidGen .= newUUIDGen
  return nextUUID
