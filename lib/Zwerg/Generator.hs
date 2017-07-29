module Zwerg.Generator
  ( module EXPORTED
  , Generator
  , Generator'
  , getRandomEmptyTile
  , assignUniformRandomStat
  , putOnRandomEmptyTile
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

type Generator = forall s m. ( HasComponents s
                             , MonadError ZError m
                             , MonadRandom m
                             , MonadState s m
                             ) => m UUID

type Generator' a = forall s m. ( HasComponents s
                                , MonadError ZError m
                                , MonadRandom m
                                , MonadState s m
                                ) => m a

getRandomEmptyTile :: UUID -> Generator' (Maybe UUID)
getRandomEmptyTile levelUUID = do
  levelTiles <- tiles <@> levelUUID
  -- TODO: make sure new tile isn't fully enclosed by walls
  unoccupiedTiles <- zFilterM (fmap not . (<@>) blocksPassage) levelTiles
  tryPickRandom unoccupiedTiles

assignUniformRandomStat :: UUID -> Stat -> (Int, Int) -> Generator' ()
assignUniformRandomStat targetUUID stat bounds = do
  newStat <- getRandomR bounds
  modComp targetUUID stats (replaceStat stat newStat)

putOnRandomEmptyTile :: UUID -> UUID -> Generator' ()
putOnRandomEmptyTile levelUUID entityUUID = do
  tileUUID <- getRandomEmptyTile levelUUID
  -- TODO modify fromJustErrM to be a template haskell'd function
  tileUUID' <-
    fromJustErrM tileUUID $
    ZError
      __FILE__
      __LINE__
      EngineFatal
      "Could not find an open tile to place Goblin"
  addComp entityUUID level levelUUID
  -- TODO: where to set tileOn? in addOccupant?
  addComp entityUUID tileOn tileUUID'
  position <@> tileUUID' >>= addComp entityUUID position
  transferOccupant entityUUID Nothing tileUUID'
