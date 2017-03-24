module Zwerg.Generator.Level.TestSquare where

import Zwerg.Generator
import Zwerg.Util
import Zwerg.Generator.Level
import Zwerg.UI.Font
import Zwerg.Generator.Enemy.Goblin

import Data.Map.Strict (traverseWithKey)

testSquareGenerator :: Generator UUID
testSquareGenerator = MkGenerator $ do
    traceM "generating Test Square..."
    testSquareLevelUUID <- getNewUUID
    generate $ levelSkeletonGenerator testSquareLevelUUID
    testSquareTiles <- demandComp tileMap testSquareLevelUUID

    _ <- flip traverseWithKey (unwrap testSquareTiles) $ \pos tileUUID -> do
        let (x,y) = unPosition pos
            isWallTile = x == 0 || x == round mapWidth - 1 || y == 0 || y == round mapHeight - 1

        if isWallTile
          then do
              setComp tileUUID tileType Wall
              setComp tileUUID glyph $ Glyph Normal 'X' $ mkColor 255 255 255
          else do
              setComp tileUUID tileType Floor
              setComp tileUUID blocked False
              setComp tileUUID glyph $ Glyph Normal '.' $ mkColor 255 255 255

    traceM "generating Goblins..."
    replicateM_ 5 $ do
      traceM $ show (__LINE__ :: Int)
      goblinUUID <- generate goblinGenerator
      traceM $ show (__LINE__ :: Int)
      goblinTileUUID <- getRandomTile testSquareLevelUUID

      traceM $ show (__LINE__ :: Int)
      goblinTileUUID' <- fromJustErrM goblinTileUUID $
        ZError __FILE__ __LINE__ Fatal "Could not find an open tile to place Player"

      traceM $ show (__LINE__ :: Int)
      demandComp position goblinTileUUID' >>= addComp goblinUUID position
      addOccupant goblinUUID goblinTileUUID'
      addComp goblinTileUUID' blocked True
      traceM $ show (__LINE__ :: Int)

    return testSquareLevelUUID
