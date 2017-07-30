module Zwerg.Event.Queue
  ( ZwergEventQueue
  , HasZwergEventQueue(..)
  , popEvent
  , pushEventM
  , mergeEventsM
  , module EXPORTED
  ) where

import Zwerg.Prelude

import Zwerg.Event as EXPORTED

import Data.Sequence (Seq, (|>), ViewL(..), (><))
import qualified Data.Sequence as S (viewl, empty, length, null)

newtype ZwergEventQueue = MkZwergEventQueue (Seq ZwergEvent)
  deriving (Show, Eq, Generic)
instance Binary ZwergEventQueue

class HasZwergEventQueue s where
  eventQueue :: Lens' s ZwergEventQueue

instance HasZwergEventQueue ZwergEventQueue where
  eventQueue = identity

instance ZWrapped ZwergEventQueue (Seq ZwergEvent) where
  unwrap (MkZwergEventQueue eq) = eq

instance ZEmptiable ZwergEventQueue where
  zEmpty  = MkZwergEventQueue S.empty
  zIsNull = S.null . unwrap
  zSize   = S.length . unwrap

popEvent :: (HasZwergEventQueue s, MonadState s m) => m (Maybe ZwergEvent)
popEvent =
  S.viewl <$> unwrap <$> use eventQueue >>= \case
    EmptyL -> return Nothing
    evt :< eq' -> do
      eventQueue .= MkZwergEventQueue eq'
      return $ Just evt

pushEventM :: (HasZwergEventQueue s, MonadState s m) => ZwergEvent -> m ()
pushEventM evt = eventQueue %= MkZwergEventQueue . (|> evt) . unwrap

mergeEventsM :: (HasZwergEventQueue s, MonadState s m) => ZwergEventQueue -> m ()
mergeEventsM evts = eventQueue %= MkZwergEventQueue . (><) (unwrap evts) . unwrap
