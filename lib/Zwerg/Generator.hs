module Zwerg.Generator
  ( module EXPORTED
  , Generator(..)
  , getRandomEmptyTile
  , assignUniformRandomStat
  ) where

import Zwerg.Component as EXPORTED
import Zwerg.Component.All as EXPORTED
import Zwerg.Data.Damage as EXPORTED
import Zwerg.Data.UUIDSet as EXPORTED
import Zwerg.Data.Equipment as EXPORTED
import Zwerg.Entity as EXPORTED
import Zwerg.Event as EXPORTED
import Zwerg.Prelude as EXPORTED
import Zwerg.Random as EXPORTED

import Control.Monad.Except as EXPORTED hiding ((<$!>))
import Control.Monad.Random as EXPORTED (MonadRandom, getRandomR)

newtype Generator a = MkGenerator
  { generate :: forall s m. ( HasComponents s
                            , MonadError ZError m
                            , MonadRandom m
                            , MonadState s m
                            ) =>
                              m a
  }

getRandomEmptyTile
  :: (HasComponents s, MonadState s m, MonadError ZError m, MonadRandom m)
  => UUID -> m (Maybe UUID)
getRandomEmptyTile levelUUID = do
  levelTiles <- demandComp tiles levelUUID
  unoccupiedTiles <- zFilterM (fmap not . demandComp blocksPassage) levelTiles
  if zIsNull unoccupiedTiles
    then return Nothing
    else Just <$> pickRandom unoccupiedTiles

assignUniformRandomStat
  :: (HasComponents s, MonadError ZError m, MonadState s m, MonadRandom m)
  => UUID -> Stat -> (Int, Int) -> m ()
assignUniformRandomStat targetUUID stat bounds = do
  newStat <- getRandomR bounds
  modComp targetUUID stats (replaceStat stat newStat)
