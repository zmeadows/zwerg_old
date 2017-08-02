{-# LANGUAGE ImpredicativeTypes #-}
module Zwerg.Component.Sketchy where

import Zwerg.Prelude

import Zwerg.Component.Base

(<@!!>) :: (Component a, Component b) -> UUID -> MonadCompState (a,b)
(<@!!>) (compA, compB) uuid = do
  x <- demandComp compA uuid
  y <- demandComp compB uuid
  return (x,y)

(<~!!>) :: (Component a, Component b) -> UUID -> MonadCompRead (a,b)
(<~!!>) (compA, compB) uuid = do
  x <- demandViewComp compA uuid
  y <- demandViewComp compB uuid
  return (x,y)
