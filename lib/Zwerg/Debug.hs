module Zwerg.Debug where

import Zwerg.Prelude

#ifdef DEBUG_CALLSTACK
import GHC.Stack (callStack, prettyCallStack)
import qualified Debug.Trace as ST (traceIO)
#endif

import System.IO.Unsafe (unsafePerformIO)
import qualified TextShow.Debug.Trace as TT (tracetIO)

--TODO print elapsed ticks
--NOTE: strictness bangs are to ensure the traces actually get printed
--Furthermore, one needs to ensure unevaluated thunks are passed in the 'msg'
--variable to gauranteed that the debug statement actually gets evaluated
debug :: (HasCallStack, Monad m) => Text -> m ()
debug !msg = return $! unsafePerformIO $! do
    TT.tracetIO "####################"
    TT.tracetIO msg
    TT.tracetIO ""
#ifdef DEBUG_CALLSTACK
    let !s = prettyCallStack callStack
    ST.traceIO s
#endif
    TT.tracetIO "####################"
    TT.tracetIO ""

