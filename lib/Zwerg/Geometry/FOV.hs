module Zwerg.Geometry.FOV (getFOV, getFOVLazy, pureGetFOVRays, getFOVSpiral) where

import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Data.Position
import Zwerg.Geometry
import Zwerg.Util
import qualified Data.List as L (nub, concat)

import Data.Ratio
import Data.IntervalMap.FingerTree (Interval(..), IntervalMap)
import qualified Data.IntervalMap.FingerTree as IM

type BlockedMap = GridMap Bool

{-# INLINABLE getFOV #-}
getFOV :: Position -> Int -> BlockedMap -> [Position]
getFOV = getFOVLazy

getFOVLazy :: Position -> Int -> BlockedMap -> [Position]
getFOVLazy pos fov _ = [ unsafeWrap (x,y) | x <- [minX..maxX], y <- [minY..maxY], inPlayerSightRange (x,y) ]
    where inPlayerSightRange (a,b) = (round $ distance Euclidean pos (unsafeWrap (a,b))) < fov
          (posX,posY) = unwrap pos
          minX = max 0 (posX - fov)
          minY = max 0 (posY - fov)
          maxX = min (mapWidthINT-1) (posX + fov)
          maxY = min (mapHeightINT-1) (posY + fov)

pureGetFOVRays :: Position -> Int -> BlockedMap -> [Position]
pureGetFOVRays pos fov blockedMap = visiblePos
    where fovEdges = circle (unwrap pos) (fov) ++ circle (unwrap pos) (fov+1)
          linesToFovEdges = map (tail . line (unwrap pos)) fovEdges
          notBlocked is = case (wrap is) of
                            Just p -> (not $ zAt blockedMap p) && (round (distance Euclidean pos p) < fov)
                            Nothing -> False
          unbrokenLines = map (takeUntil notBlocked) linesToFovEdges
          visiblePos = pos : (map unsafeWrap $ L.nub $ L.concat unbrokenLines)

--TODO: move RelativePosition to Data.Position
type RelativePosition = (Int,Int)
type AbsolutePosition = (Int,Int)
data CornerDirection  = NW | NE | SW | SE
type ShadowAngle      = Rational
type ShadowQueue      = IntervalMap ShadowAngle ()
-- type CirclePosition   = (Int, (Int, Int))
type ViewRange = Int

(.+.) :: AbsolutePosition -> RelativePosition -> Maybe Position
(.+.) (x,y) (dx,dy) = wrap (x + dx, y + dy)

getFOVSpiral :: Position -> Int -> BlockedMap -> [Position]
getFOVSpiral startPos fov bm = startPos : getFOVSpiral' (unwrap startPos) fov (squareSpiral fov) IM.empty bm []

getFOVSpiral' :: AbsolutePosition
              -> ViewRange
              -> [RelativePosition]
              -> ShadowQueue
              -> BlockedMap
              -> [Position]
              -> [Position]
getFOVSpiral' _ _ [] _  _  ps = ps
getFOVSpiral' playerPos@(x0,y0) playerViewRange ((x,y):remainingSpiral) sq bm ps = go (playerPos .+. (x,y))
  where sq' = markShadow (x,y) sq
        dR2 = (x0-x)^(2 :: Int) + (y0-y)^(2 :: Int)
        inView = dR2 <= playerViewRange ^ (2 :: Int)
        go cellPos =
            case cellPos of
              Nothing -> getFOVSpiral' playerPos playerViewRange remainingSpiral sq bm ps
              Just absPos ->
                  let alreadyBlocked = not $ null $ IM.search (shadowAngle (x,y) Nothing) sq
                      newlyBlocked = (not alreadyBlocked) && (zAt bm absPos || not inView)
                  in if newlyBlocked
                        then getFOVSpiral' playerPos playerViewRange remainingSpiral sq' bm (absPos:ps)
                        else if alreadyBlocked
                                then getFOVSpiral' playerPos playerViewRange remainingSpiral sq' bm ps
                                else getFOVSpiral' playerPos playerViewRange remainingSpiral sq bm (absPos:ps)

markShadow :: RelativePosition -> ShadowQueue -> ShadowQueue
markShadow r@(dx, dy) sq =
    if | dx == 0 && dy == 0 -> sq
       | dx > 0 && dy > 0   -> go (Just SE) (Just NW) sq
       | dx < 0 && dy > 0   -> go (Just NE) (Just SW) sq
       | dx > 0 && dy < 0   -> go (Just SW) (Just NE) sq
       | dx < 0 && dy < 0   -> go (Just NW) (Just SE) sq
       | dx == 0 && dy > 0  -> go (Just SE) (Just SW) sq
       | dx == 0 && dy < 0  -> go (Just NW) (Just NE) sq
       | dx < 0 && dy == 0  -> go (Just NE) (Just SE) sq
       | dx > 0 && dy == 0  -> go (Just SW) Nothing $ go Nothing (Just NW) sq
  where go d1 d2 = IM.insert (Interval (shadowAngle r d1) (shadowAngle r d2)) ()

shadowAngle :: RelativePosition -> Maybe CornerDirection -> ShadowAngle
shadowAngle (dx, dy) corner =
    case corner of
      Just NW -> go (-0.5) 0.5
      Just NE -> go 0.5 0.5
      Just SW -> go (-0.5) (-0.5)
      Just SE -> go 0.5 (-0.5)
      Nothing -> go 0.0 0.0
  where minAngle = 1.0 / (pi * 1e6)
        dyd = fromIntegral dy :: Double
        dxd = fromIntegral dx :: Double
        go dx2 dy2 = approxRational (pi + atan2 (dyd + dy2) (dxd + dx2)) minAngle

