module Zwerg.Generator
  ( module EXPORTED
  , Generator
  , Generator'
  , getRandomEmptyTile
  , assignUniformRandomStat
  , putOnRandomEmptyTile
  , generateAndEquip
  , generateAndHold
  , generateAndHoldN
  ) where

import Zwerg.Component as EXPORTED
import Zwerg.Data.Damage as EXPORTED
import Zwerg.Data.Equipment as EXPORTED
import Zwerg.Data.GridMap as EXPORTED
import Zwerg.Data.HP as EXPORTED
import Zwerg.Data.Position as EXPORTED
import Zwerg.Data.UUIDSet as EXPORTED
import Zwerg.Debug as EXPORTED
import Zwerg.Entity as EXPORTED
import Zwerg.Event as EXPORTED
import Zwerg.Prelude as EXPORTED
import Zwerg.Random as EXPORTED
import Zwerg.Util as EXPORTED

import Control.Monad.Random as EXPORTED (MonadRandom, getRandomR)
import Data.Text(append)
import Language.Haskell.TH


type Generator' a = forall s m. ( HasCallStack
                                , HasComponents s
                                , MonadRandom m
                                , MonadState s m
                                ) => m a

type Generator = Generator' UUID

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
  getRandomEmptyTile levelUUID >>= \case
      Just tileUUID -> do
          addComp entityUUID level levelUUID
          transferOccupant entityUUID Nothing tileUUID
      Nothing -> debug $ "Couldn't find empty tile to place entity with UUID: "
                         <> show entityUUID

-- verify :: String -> String -> Q Exp
-- verify us cs = [| verifyComponent $(dyn cs) $(dyn us) |]
-- 
-- hasAll :: String -> [String] -> Q Exp
-- hasAll us css = DoE <$> (mapM (fmap NoBindS . verify us) css)

generateAndEquip :: Generator -> UUID -> Generator' ()
--TODO: check if slot is already filled (or wait... do this in equipItem?)
generateAndEquip itemGen wearerUUID = itemGen >>= (flip equipItem) wearerUUID

generateAndHold :: Generator -> UUID -> Generator' ()
generateAndHold itemGen wearerUUID = (zAdd <$> itemGen) >>= modComp wearerUUID inventory

generateAndHoldN :: Int -> Generator -> UUID -> Generator' ()
generateAndHoldN n itemGen wearerUUID = replicateM_ n $ generateAndHold itemGen wearerUUID
