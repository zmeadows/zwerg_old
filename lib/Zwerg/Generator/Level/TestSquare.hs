module Zwerg.Generator.Level.TestSquare (testSquare) where

import Zwerg.Generator
import Zwerg.Generator.Default
-- import Zwerg.Generator.Verify
-- import Zwerg.Generator.Enemy.Goblin
-- import Zwerg.Generator.Item.Weapon

testSquare :: Generator
testSquare = Generator testSquareHatch []
    -- replicateM_ 5 $ goblin >>= putOnRandomEmptyTile testSquareLevelUUID
    -- replicateM_ 4 $ sword >>= putOnRandomEmptyTile testSquareLevelUUID


testSquareHatch :: EntityHatcher
testSquareHatch = MkEntityHatcher $ do
    testSquareLevelUUID <- generateSkeleton Level
    testSquareTiles <- tileMap <@> testSquareLevelUUID

    let wallGlyph = Glyph 'X' $ CellColor white $ Just black
        floorGlyph = Glyph '·' $ CellColor white $ Just black

    zTraverseWithKey_ testSquareTiles $ \pos tileUUID -> do
        let (x, y) = unwrap pos
            isWallTile = x == 0 || x == mapWidthINT - 1 || y == 0 || y == mapHeightINT - 1
            (<@-) :: Component a -> a -> MonadCompState ()
            (<@-) = setComp tileUUID
        if isWallTile
          then do
            tileType      <@- Wall
            blocksPassage <@- True
            blocksVision  <@- True
            glyph         <@- wallGlyph
            name          <@- "Wall tile in the test level"
            description   <@- "It is a wall."
          else do
            tileType      <@- Floor
            blocksPassage <@- False
            blocksVision  <@- False
            glyph         <@- floorGlyph
            name          <@- "Floor tile in the test level"
            description   <@- "It is a floor."

    return testSquareLevelUUID
