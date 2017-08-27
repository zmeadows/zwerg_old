module Zwerg.Generator.Stairs (buildRandomStairs) where

import Zwerg.Generator
-- import Zwerg.Generator.Default
import Zwerg.Generator.Verify

buildRandomStairs :: UUID -> UUID -> Generator' ()
buildRandomStairs lowerLevelUUID upperLevelUUID = do
   inM2 getRandomEmptyTile (lowerLevelUUID, upperLevelUUID) >>= \case
       (Just lowerStairsUUID, Just upperStairsUUID) -> do
           addComp lowerStairsUUID name        "Stone staircase"
           addComp lowerStairsUUID description "A carved stone staircase."
           addComp lowerStairsUUID glyph       (Glyph '<' zDefault)
           addComp lowerStairsUUID tileType    (Stairs Up)

           addComp upperStairsUUID name        "Stone staircase"
           addComp upperStairsUUID description "A carved stone staircase."
           addComp upperStairsUUID glyph       (Glyph '>' zDefault)
           addComp upperStairsUUID tileType    (Stairs Down)

           addComp lowerStairsUUID connectedTo upperStairsUUID
           addComp upperStairsUUID connectedTo lowerStairsUUID

           setComp lowerStairsUUID blocksVision True

           void $ inM2 verifyAndReturn (lowerStairsUUID, upperStairsUUID)
       (_,_) -> debug "Could not find two empty tiles to place stairs on."

