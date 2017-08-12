{-# LANGUAGE ImpredicativeTypes #-}
module Zwerg.Component.Sketchy where

import Zwerg.Prelude
import Zwerg.Component.Base

(<@!!>) :: (ZDefault a, ZDefault b)
        => (Component a, Component b) -> UUID -> MonadCompState (a,b)
(<@!!>) (compA, compB) uuid = do
  x <- demandComp compA uuid
  y <- demandComp compB uuid
  return (x,y)

(<@@!>) :: ZDefault a => Component a -> (UUID,UUID) -> MonadCompState (a,a)
(<@@!>) comp (uuidA,uuidB) = do
  x <- demandComp comp uuidA
  y <- demandComp comp uuidB
  return (x,y)

(<~!!>) :: (ZDefault a, ZDefault b)
        => (Component a, Component b) -> UUID -> MonadCompRead (a,b)
(<~!!>) (compA, compB) uuid = do
  x <- demandViewComp compA uuid
  y <- demandViewComp compB uuid
  return (x,y)

(<~!!!>) :: (ZDefault a, ZDefault b, ZDefault c)
         => (Component a, Component b, Component c) -> UUID -> MonadCompRead (a,b,c)
(<~!!!>) (compA, compB, compC) uuid = do
  x <- demandViewComp compA uuid
  y <- demandViewComp compB uuid
  z <- demandViewComp compC uuid
  return (x,y,z)

(<~~!>) :: ZDefault a => Component a -> (UUID,UUID) -> MonadCompRead (a,a)
(<~~!>) comp (uuidA,uuidB) = do
  x <- demandViewComp comp uuidA
  y <- demandViewComp comp uuidB
  return (x,y)

(<@@=>) :: ZDefault a => Component a -> (UUID,UUID) -> MonadCompState ()
(<@@=>) comp (uuidA,uuidB) = demandComp comp uuidA >>= setComp uuidB comp

(<@@==>) :: (ZDefault a, ZDefault b)
         => (Component a, Component b) -> (UUID,UUID) -> MonadCompState ()
(<@@==>) (compA,compB) (uuidA,uuidB) = do
  demandComp compA uuidA >>= setComp uuidB compA
  demandComp compB uuidA >>= setComp uuidB compB

(<@@===>) :: (ZDefault a, ZDefault b, ZDefault c)
          => (Component a, Component b, Component c) -> (UUID,UUID) -> MonadCompState ()
(<@@===>) (compA,compB,compC) (uuidA,uuidB) = do
  demandComp compA uuidA >>= setComp uuidB compA
  demandComp compB uuidA >>= setComp uuidB compB
  demandComp compC uuidA >>= setComp uuidB compC
