module Zwerg.Generator.Level.TestSquare where

import Zwerg.Generator
import Zwerg.Generator.Enemy.Goblin
import Zwerg.Generator.Item.Weapon
import Zwerg.Generator.Level

testSquareGenerator :: Generator
testSquareGenerator = do
    testSquareLevelUUID <- levelSkeletonGenerator
    testSquareTiles <- tileMap <@> testSquareLevelUUID
    traverseWithPos_ testSquareTiles $ \pos tileUUID -> do
        let (x, y) = unwrap pos
            isWallTile = x == 0 || x == mapWidthINT - 1 || y == 0 || y == mapHeightINT - 1
        if isWallTile
          then do
            setComp tileUUID tileType Wall
            setComp tileUUID blocksPassage True
            setComp tileUUID glyph $ Glyph 'X' White2 White0 (Just Black2) (Just Black0)
            setComp tileUUID name "Wall tile"
            setComp tileUUID description "It is a wall."
          else do
            setComp tileUUID tileType Floor
            setComp tileUUID blocksPassage False
            setComp tileUUID glyph $ Glyph '·' White2 White0 (Just Black2) (Just Black0)
            setComp tileUUID name "Floor tile"
            setComp tileUUID description "It is a floor."
    replicateM_ 5 $ goblin >>= putOnRandomEmptyTile testSquareLevelUUID
    replicateM_ 4 $ sword >>= putOnRandomEmptyTile testSquareLevelUUID
    return testSquareLevelUUID


