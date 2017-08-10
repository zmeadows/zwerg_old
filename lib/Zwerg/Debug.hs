module Zwerg.Debug where

import Zwerg.Prelude

import GHC.Stack (callStack, prettyCallStack)
import System.IO.Unsafe (unsafePerformIO)
import qualified TextShow.Debug.Trace as TT (tracetIO)
import qualified Debug.Trace as ST (traceIO)

--TODO print elapsed ticks
--NOTE: strictness bangs are to ensure the traces actually get printed
--Furthermore, one needs to ensure unevaluated thunks are passed in the 'msg'
--variable to gauranteed that the debug statement actually gets evaluated
debug :: (HasCallStack, Monad m) => Text -> m ()
debug !msg = return $! unsafePerformIO $! do
    let !s = prettyCallStack callStack
    TT.tracetIO "####################"
    TT.tracetIO msg
    TT.tracetIO ""
    ST.traceIO s
    TT.tracetIO "####################"
    TT.tracetIO ""

