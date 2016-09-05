module Zwerg.Generator.Level.TestSquare where

import Zwerg.Generator
import Zwerg.Generator.Level

import Control.Lens (makeLenses, use, to, view)
import Control.Monad (forM_)

data TestSquareGenContext = TestSquareGenContext
    { _testSquareLevelUUID :: UUID
    } deriving (Show, Read, Eq)
makeLenses ''TestSquareGenContext

instance LevelGenContext TestSquareGenContext where
    levelUUID = testSquareLevelUUID

type TestSquareGenerator = Generator TestSquareGenContext

generateTestSquare :: TestSquareGenerator ()
generateTestSquare = do
    genLevelSkeleton
    lid <- view levelUUID
    ltiles <- demandComp lid tiles

    let addTileComp p = addComp $ getTileUUIDAtPos p ltiles

    forM_ wallPositions $ \pos -> do
        addTileComp pos tileType Wall
        addTileComp pos glyph $ mkGlyph 'X' $ mkColor 255 255 255

    forM_ floorPositions $ \pos -> do
        addTileComp pos tileType Floor
        addTileComp pos blocked False
        addTileComp pos glyph $ mkGlyph '.' $ mkColor 255 255 255

wallPositions :: [Position]
wallPositions = [ mkPosition (x,y)
                    | x <- [0, mapWidth - 1]
                    , y <- [0,mapHeight - 1]
                ]

floorPositions :: [Position]
floorPositions = [ mkPosition (x,y)
                     | x <- [1..mapWidth - 2]
                     , y <- [1..mapHeight - 2]
                 ]
