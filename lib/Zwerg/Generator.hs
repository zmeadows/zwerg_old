module Zwerg.Generator
  ( module EXPORTED
  , Generator(..)
  , generate
  , EntityAssembler(..)
  , EntityHatcher(..)
  , (+>)
  , (<+)
  , (++>)
  , (<++)
  , assignUniformRandomStats
  , putOnRandomEmptyTile
  , generateOnRandomEmptyTile
  , generateAndEquip
  , generateAndHold
  , generateAndHoldN
  ) where

import Zwerg.Component      as EXPORTED
import Zwerg.Data.Damage    as EXPORTED
import Zwerg.Data.Equipment as EXPORTED
import Zwerg.Data.GridMap   as EXPORTED
import Zwerg.Data.Glyph     as EXPORTED
import Zwerg.Data.HP        as EXPORTED
import Zwerg.Data.ZColor    as EXPORTED
import Zwerg.Data.Position  as EXPORTED
import Zwerg.Data.UUIDSet   as EXPORTED
import Zwerg.Debug          as EXPORTED
import Zwerg.Entity         as EXPORTED
import Zwerg.Event          as EXPORTED
import Zwerg.Prelude        as EXPORTED
import Zwerg.Random         as EXPORTED
import Zwerg.Util           as EXPORTED

import Control.Monad.Random as EXPORTED (MonadRandom, getRandomR)

data EntityHatcher = MkEntityHatcher (MonadCompStateRand UUID)
data EntityAssembler = MkEntityAssembler (UUID -> MonadCompStateRand ())
data Generator = Generator EntityHatcher [EntityAssembler]

(<+) :: Generator -> EntityAssembler -> Generator
(<+) (Generator hatcher assemblers) a = Generator hatcher (a:assemblers)

(+>) :: Generator -> EntityAssembler -> Generator
(+>) (Generator hatcher assemblers) a = Generator hatcher (assemblers ++ [a])

(++>) :: Generator -> [EntityAssembler] -> Generator
(++>) (Generator hatcher assemblers) newAssemblers = Generator hatcher (assemblers ++ newAssemblers)

(<++) :: Generator -> [EntityAssembler] -> Generator
(<++) (Generator hatcher assemblers) newAssemblers = Generator hatcher (newAssemblers ++ assemblers)

generate :: Generator -> MonadCompStateRand UUID
generate (Generator (MkEntityHatcher hatch) assemblers) = do
    newEntityUUID <- hatch
    forM_ assemblers $ \(MkEntityAssembler f) -> f newEntityUUID
    return newEntityUUID

putOnRandomEmptyTile :: UUID -> EntityAssembler
putOnRandomEmptyTile levelUUID = MkEntityAssembler $ \entityUUID -> do
    (rcr $ getRandomEmptyTileR levelUUID) >>= \case
        Just tileUUID -> do
            addComp entityUUID level levelUUID
            transferOccupant entityUUID Nothing tileUUID
        Nothing -> debug $ "Couldn't find empty tile to place entity with UUID: "
                           <> show (unwrap entityUUID)

generateOnRandomEmptyTile :: Generator -> EntityAssembler
generateOnRandomEmptyTile gen = MkEntityAssembler $ \levelUUID ->
    void $ generate $ gen +> putOnRandomEmptyTile levelUUID

assignUniformRandomStats :: [(Stat, (Int, Int))] -> EntityAssembler
assignUniformRandomStats ss = MkEntityAssembler $ \entityUUID ->
    forM_ ss $ \(stat, bounds) -> do
        newStat <- getRandomR bounds
        modComp entityUUID stats (replaceStat stat newStat)

--TODO: check if slot is already filled and add to inventory if it is
generateAndEquip :: Generator -> EntityAssembler
generateAndEquip itemGen = MkEntityAssembler $ \wearerUUID ->
    generate itemGen >>= (flip equipItem) wearerUUID

generateAndHold :: Generator -> EntityAssembler
generateAndHold itemGen = MkEntityAssembler $ \wearerUUID ->
    (zAdd <$> generate itemGen) >>= modComp wearerUUID inventory

generateAndHoldN :: Int -> Generator -> EntityAssembler
generateAndHoldN n itemGen = MkEntityAssembler $ \wearerUUID ->
    replicateM_ n $ (zAdd <$> generate itemGen) >>= modComp wearerUUID inventory
