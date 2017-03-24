module Zwerg.Generator (
    module EXPORTED,
    Generator(..),
    getRandomTile
    ) where

import Zwerg.Prelude         as EXPORTED
import Zwerg.Class           as EXPORTED
import Zwerg.Const           as EXPORTED
import Zwerg.Component       as EXPORTED
import Zwerg.Component.All   as EXPORTED
import Zwerg.Behavior        as EXPORTED
import Zwerg.Data.Color      as EXPORTED
import Zwerg.Data.UUIDSet    as EXPORTED
import Zwerg.Data.RanGen     as EXPORTED
import Zwerg.Data.Error      as EXPORTED

import Control.Monad.Random       as EXPORTED (MonadRandom, getRandomR)
import Control.Monad.Except       as EXPORTED hiding ((<$!>))

newtype Generator a = MkGenerator {
  generate :: forall s m.
    ( HasComponents s,
      HasBehaviors s,
      HasUUIDGen s,
      MonadError ZError m,
      MonadRandom m,
      MonadState s m
    ) => m a
  }

getRandomTile :: (HasComponents s, MonadState s m, MonadError ZError m, MonadRandom m)
              => UUID -> m (Maybe UUID)
getRandomTile levelUUID = do
  levelTiles <- demandComp tiles levelUUID
  unoccupiedTiles <- zFilterM (fmap not . demandComp blocked) levelTiles
  if (zIsNull unoccupiedTiles)
    then return Nothing
    else Just <$> pickRandom unoccupiedTiles
