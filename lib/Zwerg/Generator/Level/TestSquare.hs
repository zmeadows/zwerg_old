module Zwerg.Generator.Level.TestSquare where

import Zwerg.Entity
import Zwerg.Generator
import Zwerg.Generator.Enemy.Goblin
import Zwerg.Generator.Level
import Zwerg.Util

import Data.Map.Lazy (traverseWithKey)

testSquareGenerator :: Generator UUID
testSquareGenerator = MkGenerator $ do
    testSquareLevelUUID <- popUUID
    generate $ levelSkeletonGenerator testSquareLevelUUID
    testSquareTiles <- demandComp tileMap testSquareLevelUUID
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
    replicateM_ 150 $ do
      goblinUUID <- generate goblin
      -- TODO factor out function to add any generated enemy UUID to random open tile
      addComp goblinUUID level testSquareLevelUUID
      goblinTileUUID <- getRandomEmptyTile testSquareLevelUUID
      -- TODO modify fromJustErrM to be a template haskell'd function
      goblinTileUUID' <-
        fromJustErrM goblinTileUUID $
        ZError
          __FILE__
          __LINE__
          EngineFatal
          "Could not find an open tile to place Goblin"
      demandComp position goblinTileUUID' >>= addComp goblinUUID position
      addOccupant goblinUUID goblinTileUUID'

    return testSquareLevelUUID
