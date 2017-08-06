module Zwerg.Debug where

import Zwerg.Prelude

import GHC.Stack (callStack, prettyCallStack)
import Debug.Trace (traceShowM)

debug :: Applicative f => Text -> f ()
debug msg =
    traceShowM ("####################" :: Text)
    *> traceShowM msg
    *> traceShowM ("" :: Text)
    *> traceShowM ("CALL STACK: " :: Text)
    *> traceShowM $ prettyCallStack callStack
    *> traceShowM ("####################" :: Text)
    *> traceShowM ("" :: Text)

