module Zwerg.Generator.Level.TestSquare where

import Zwerg.Generator
import Zwerg.Generator.Enemy.Goblin
import Zwerg.Generator.Level

import Data.Map.Lazy (traverseWithKey)

testSquareGenerator :: Generator
testSquareGenerator = MkGenerator $ do
    testSquareLevelUUID <- popUUID
    generate $ levelSkeletonGenerator testSquareLevelUUID
    testSquareTiles <- tileMap <@> testSquareLevelUUID
    void $ flip traverseWithKey (unwrap testSquareTiles) $ \pos tileUUID -> do
        let (x, y) = unwrap pos
            isWallTile = x == 0 || x == mapWidthINT - 1 || y == 0 || y == mapHeightINT - 1
        if isWallTile
          then do
            setComp tileUUID tileType Wall
            setComp tileUUID blocksPassage True
            setComp tileUUID glyph $ Glyph 'X' White2 White0 (Just Black2) (Just Black0)
          else do
            setComp tileUUID tileType Floor
            setComp tileUUID blocksPassage False
            setComp tileUUID glyph $ Glyph '·' White2 White0 (Just Black2) (Just Black0)
    replicateM_ 150 $ putEntityOnRandomEmptyTile testSquareLevelUUID goblin
    return testSquareLevelUUID
