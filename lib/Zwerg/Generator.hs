module Zwerg.Generator
  ( module EXPORTED
  , Generator
  , Generator'
  , getRandomEmptyTile
  , assignUniformRandomStat
  , putOnRandomEmptyTile
  , hasAll
  , generateAndEquip
  ) where

import Zwerg.Component as EXPORTED
import Zwerg.Data.HP as EXPORTED
import Zwerg.Data.Position as EXPORTED
import Zwerg.Data.GridMap as EXPORTED
import Zwerg.Data.Damage as EXPORTED
import Zwerg.Data.UUIDSet as EXPORTED
import Zwerg.Data.Equipment as EXPORTED
import Zwerg.Entity as EXPORTED
import Zwerg.Event as EXPORTED
import Zwerg.Prelude as EXPORTED
import Zwerg.Random as EXPORTED
import Zwerg.Util as EXPORTED

import Language.Haskell.TH

import Control.Monad.Random as EXPORTED (MonadRandom, getRandomR)

type Generator = forall s m. ( HasCallStack
                             , HasComponents s
                             , MonadError ZError m
                             , MonadRandom m
                             , MonadState s m
                             ) => m UUID

type Generator' a = forall s m. ( HasCallStack
                                , HasComponents s
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
  tileUUID <- getRandomEmptyTile levelUUID >>= $(maybeThrow) EngineFatal "Couldn't find empty tile to place entity"
  addComp entityUUID level levelUUID
  transferOccupant entityUUID Nothing tileUUID

verify :: String -> String -> Q Exp
verify us cs = [| demandCanViewComp $(dyn cs) $(dyn us) |]

hasAll :: String -> [String] -> Q Exp
hasAll us css = DoE <$> (mapM (fmap NoBindS . verify us) css)

generateAndEquip :: Generator -> UUID -> Generator' ()
generateAndEquip itemGen wearerUUID = itemGen >>= (flip equipItem) wearerUUID


