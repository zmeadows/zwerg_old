module Zwerg.Entity.AI
  ( runAI
  ) where

import Zwerg.Component
import Zwerg.Component.All
import Zwerg.Event
import Zwerg.Prelude
import Zwerg.Random

import Control.Monad.Random (RandT, evalRandT)

newtype AI a =
  AI (ExceptT ZError (RandT RanGen (StateT ZwergEventQueue (Reader Components))) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Components
           , MonadError ZError
           , MonadState ZwergEventQueue
           , MonadRandom
           )

runAI
  :: ( HasComponents s
     , HasZwergEventQueue s
     , MonadError ZError m
     , MonadState s m
     , MonadSplit RanGen m
     )
  => UUID -> m ()
runAI uuid = do
  cmps <- use components
  ait <- demandComp aiType uuid
  gen <- getSplit
  let (AI a) = enact uuid ait
      (err, evts) =
        runReader (runStateT (evalRandT (runExceptT a) gen) zEmpty) cmps
  case err of
    Left zErr -> throwError zErr
    Right () -> mergeEventsM evts

enact :: UUID -> AIType -> AI ()
--enact entityUUID SimpleMeleeCreature =
--  modify . pushEvent $ MoveEntityEvent $ MoveEntityEventData 0
enact _ _ = return ()
