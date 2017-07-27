module Zwerg.Entity.AI
  ( runAI
  ) where

import Zwerg.Component
import Zwerg.Component.Position
import Zwerg.Entity
import Zwerg.Event
import Zwerg.Prelude
import Zwerg.Random

import Control.Monad.Loops (minimumByM)
import Control.Monad.Random (RandT, evalRandT)
import Data.Maybe (fromJust)

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
  ait <- aiType <@> uuid
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
  possTiles <- catMaybes <$> mapM (`getAdjacentTileUUID` tileUUID) cardinalDirections
  openPossTiles <- filterM (\i -> not <$> tileBlocksPassage i) possTiles
  playerPos <- position <~> playerUUID
  let distanceToPlayer e1UUID e2UUID = do
        e1Pos <- position <~> e1UUID
        e2Pos <- position <~> e2UUID
        return $ compare (distance Euclidean e1Pos playerPos)
                         (distance Euclidean e2Pos playerPos)
  unless (null openPossTiles) $ do
    newTileUUID <- fromJust <$> minimumByM distanceToPlayer openPossTiles
    newPos <- position <~> newTileUUID
    modify . pushEvent $ MoveEntityEvent $ MoveEntityEventData entityUUID newPos

enact _ _ = return ()
