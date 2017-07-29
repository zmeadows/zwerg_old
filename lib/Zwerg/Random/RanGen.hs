module Zwerg.Random.RanGen
  ( RanGen
  , RanChoice(..)
  , RanChoiceFail(..)
  , newPureRanGen
  , pureRanGen
  ) where

import Zwerg.Data.UUIDSet
import Zwerg.Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word64)
import System.Random.Mersenne.Pure64 (PureMT, pureMT, newPureMT)

import qualified Data.List.NonEmpty as NE

type RanGen = PureMT

pureRanGen :: Word64 -> RanGen
pureRanGen = pureMT

newPureRanGen :: (MonadIO m) => m RanGen
newPureRanGen = liftIO newPureMT

class RanChoiceFail a b | a -> b where
  tryPickRandom :: (MonadRandom m) => a -> m (Maybe b)

class RanChoice a b | a -> b where
  pickRandom :: (MonadRandom m) => a -> m b

instance RanChoice (NonEmpty a) a where
  pickRandom xs =
    let maxInd = NE.length xs - 1
    in (NE.!!) xs <$> getRandomR (0, maxInd)

instance RanChoiceFail UUIDSet UUID where
  tryPickRandom us = case unwrap us of
                       [] -> return Nothing
                       x:xs -> fmap Just $ pickRandom $ x :| xs

instance RanChoiceFail [a] a where
  tryPickRandom xs = case xs of
                       [] -> return Nothing
                       x:xss -> fmap Just $ pickRandom $ x :| xss
