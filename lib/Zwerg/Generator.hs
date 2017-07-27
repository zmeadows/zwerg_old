module Zwerg.Generator
  ( module EXPORTED
  , Generator'(..)
  , Generator
  , getRandomEmptyTile
  , assignUniformRandomStat
  , putEntityOnRandomEmptyTile
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
import Zwerg.Util as EXPORTED

import Control.Monad.Except as EXPORTED hiding ((<$!>))
import Control.Monad.Random as EXPORTED (MonadRandom, getRandomR)

newtype Generator' a = MkGenerator
  { generate :: forall s m. ( HasComponents s
                            , MonadError ZError m
                            , MonadRandom m
                            , MonadState s m
                            ) => m a
  }

type Generator = Generator' UUID

getRandomEmptyTile
  :: (HasComponents s, MonadState s m, MonadError ZError m, MonadRandom m)
  => UUID -> m (Maybe UUID)
getRandomEmptyTile levelUUID = do
  levelTiles <- tiles <@> levelUUID
  -- TODO: make sure new tile isn't fully enclosed by walls
  unoccupiedTiles <- zFilterM (fmap not . (<@>) blocksPassage) levelTiles
  if zIsNull unoccupiedTiles
    then return Nothing
    else Just <$> pickRandom unoccupiedTiles

assignUniformRandomStat
  :: (HasComponents s, MonadState s m, MonadRandom m)
  => UUID -> Stat -> (Int, Int) -> m ()
assignUniformRandomStat targetUUID stat bounds = do
  newStat <- getRandomR bounds
  modComp targetUUID stats (replaceStat stat newStat)

putEntityOnRandomEmptyTile
  :: (HasComponents s, MonadState s m, MonadError ZError m, MonadRandom m)
  => UUID -> Generator -> m ()
putEntityOnRandomEmptyTile levelUUID entityGen = do
  newEntityUUID <- generate entityGen
  tileUUID <- getRandomEmptyTile levelUUID
  -- TODO modify fromJustErrM to be a template haskell'd function
  tileUUID' <-
    fromJustErrM tileUUID $
    ZError
      __FILE__
      __LINE__
      EngineFatal
      "Could not find an open tile to place Goblin"
  addComp newEntityUUID level levelUUID
  position <@> tileUUID' >>= addComp newEntityUUID position
  addOccupant newEntityUUID tileUUID'
