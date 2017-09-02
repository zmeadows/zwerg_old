module Zwerg.Generator.Level.Cave (cave) where

import Zwerg.Generator
import Zwerg.Generator.Default
-- import Zwerg.Generator.Verify
-- import Zwerg.Generator.Enemy.Goblin
-- import Zwerg.Generator.Item.Weapon

import qualified Data.Vector.Primitive as IV
import qualified Data.Vector.Primitive.Mutable as MV

import Control.Monad.ST
import Control.Monad.Primitive

cave :: Generator
cave = Generator caveHatch []

type CaveCellType = Int

blocked, open :: CaveCellType
blocked = 1
open = 0

buildRandomStartCells :: MonadCompReadRand (GridMap CaveCellType)
buildRandomStartCells = zBuildM $ const $ do
    r <- getRandomR (0.0, 1.0) :: MonadCompReadRand Double
    if r > 0.55
       then return blocked
       else return open

caveHatch :: EntityHatcher
caveHatch = MkEntityHatcher $ do
    q <- IV.convert . unwrap <$> rcr buildRandomStartCells
    let qs = runST $ automate q

    testSquareLevelUUID <- generateSkeleton Level

    testSquareTiles <- tileMap <@> testSquareLevelUUID

    zTraverseWithKey_ testSquareTiles $ \pos tileUUID -> do
        let isWallTile = (qs IV.! to1DIndex pos) == blocked
            (<.-) :: Component a -> a -> MonadCompState ()
            (<.-) = setComp tileUUID
        if isWallTile
          then do
            tileType      <.- Wall
            blocksPassage <.- True
            blocksVision  <.- True
            glyph         <.- Glyph 'X' (CellColor white $ Just darkslateblue)
            name          <.- "Wall tile in the test level"
            description   <.- "It is a wall."
          else do
            tileType      <.- Floor
            blocksPassage <.- False
            blocksVision  <.- False
            glyph         <.- Glyph '·' (CellColor white $ Just darkslateblue)
            name          <.- "Floor tile in the test level"
            description   <.- "It is a floor."

    return testSquareLevelUUID


--FIXME: replace weird type signatures with just ST?

automate :: HasCallStack => IV.Vector CaveCellType -> ST s (IV.Vector CaveCellType)
automate v = do
    blockVec <- IV.unsafeThaw v
    --FIXME: use a fold, dummy
    blockVec' <- iterateCaveFormation blockVec
    blockVec'' <- iterateCaveFormation blockVec'
    blockVec''' <- iterateCaveFormation blockVec''
    blockVec'''' <- iterateCaveFormation blockVec'''
    blockVec''''' <- iterateCaveFormation blockVec''''
    IV.unsafeFreeze blockVec'''''

isCellBlocked :: HasCallStack => MV.MVector (PrimState (ST s)) CaveCellType -> Int -> ST s Bool
isCellBlocked mv i =
    if i < 0 || i >= MV.length mv
       then return True
       else (== blocked) <$> MV.read mv i

countBlockedNeighbors :: HasCallStack => MV.MVector (PrimState (ST s)) CaveCellType -> Int -> ST s Int
countBlockedNeighbors mv i =
    --FIXME: just do mutable STRef style
    length <$> filterM (isCellBlocked mv)
              [ i, i+1, i-1
              , i+mapWidthINT
              , i-mapWidthINT
              , i+mapWidthINT+1
              , i-mapWidthINT+1
              , i+mapWidthINT-1
              , i-mapWidthINT-1 ]

iterateCaveFormation :: HasCallStack
                     => MV.MVector (PrimState (ST s)) CaveCellType
                     -> ST s (MV.MVector (PrimState (ST s)) CaveCellType)
iterateCaveFormation mv = do
    --FIXME: keep edges walled off
    nextIterVec <- MV.replicate (MV.length mv) open
    forM_ [0 .. MV.length mv - 1] $ \i -> do
        numNeighbors <- countBlockedNeighbors mv i
        if numNeighbors >= 5
           then MV.unsafeWrite nextIterVec i blocked
           else MV.unsafeWrite nextIterVec i open
    return nextIterVec


