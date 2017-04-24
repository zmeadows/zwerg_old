module Zwerg.Entity.AI
  ( runAI
  ) where

import Zwerg.Component
import Zwerg.Entity
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
     , MonadRandom m
     )
  => UUID -> m ()
runAI uuid = do
  cmps <- use components
  ait <- demandComp aiType uuid
  ranWord <- getRandom
  let (AI a) = enact uuid ait
      (err, evts) =
        runReader
          (runStateT (evalRandT (runExceptT a) $ pureRanGen ranWord) zEmpty)
          cmps
  case err of
    Left zErr -> throwError zErr
    Right () -> mergeEventsM evts

enact :: UUID -> AIType -> AI ()
enact entityUUID SimpleMeleeCreature = do
  tileUUID <- getEntityTileUUID entityUUID
  possTiles <-
    catMaybes <$>
    mapM (`getAdjacentTileUUID` tileUUID) [North, South, East, West]
  openPossTiles <- filterM (\i -> not <$> tileBlocksPassage i) possTiles
  unless (null openPossTiles) $ do
    ranTileUUID <- pickRandom openPossTiles
    newPos <- demandViewComp position ranTileUUID
    modify . pushEvent $ MoveEntityEvent $ MoveEntityEventData entityUUID newPos
enact _ _ = return ()
