module Zwerg.Geometry.FOV (getFOV, getFOVLazy, pureGetFOVRays) where

import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Data.Position
import Zwerg.Geometry
import Zwerg.Util
import qualified Data.List as L (nub, concat)

-- import Data.Ratio
-- import Data.IntervalMap.FingerTree (Interval(..), IntervalMap(..))
-- import qualified Data.IntervalMap.FingerTree as IM

type BlockedMap = GridMap Bool
-- type RelativePosition = (Int,Int)

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

-- 
-- type ShadowInterval = (ShadowAngle, ShadowAngle)


-- data CornerDirection = NW | NE | SW | SE
-- type ShadowAngle = Rational
-- type ShadowQueue = IntervalMap ShadowAngle ()
-- 
-- markShadow :: RelativePosition -> ShadowQueue -> ShadowQueue
-- markShadow r@(dx, dy) sq =
--     if | dx == 0 && dy == 0 -> IM.insert (Interval (0 % 1) (360 % 1)) () sq
--        | dx == 0 && dy > 0  -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx == 0 && dy < 0  -> IM.insert (Interval (shadowAngle r NW) (shadowAngle r NE)) () sq
--        | dx > 0 && dy == 0  -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx < 0 && dy == 0  -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx > 0 && dy > 0   -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx < 0 && dy > 0   -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx > 0 && dy < 0   -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
--        | dx < 0 && dy < 0   -> IM.insert (Interval (shadowAngle r SE) (shadowAngle r SW)) () sq
-- 
-- shadowAngle :: RelativePosition -> CornerDirection -> ShadowAngle
-- shadowAngle (dx, dy) NW = approxRational (atan $ (fromIntegral dx - 0.5) / (fromIntegral dy + 0.5)) (1.0/(360.0 * 10))
-- shadowAngle (dx, dy) NE = approxRational (atan $ (fromIntegral dx + 0.5) / (fromIntegral dy + 0.5)) (1.0/(360.0 * 10))
-- shadowAngle (dx, dy) SW = approxRational (atan $ (fromIntegral dx - 0.5) / (fromIntegral dy - 0.5)) (1.0/(360.0 * 10))
-- shadowAngle (dx, dy) SE = approxRational (atan $ (fromIntegral dx + 0.5) / (fromIntegral dy - 0.5)) (1.0/(360.0 * 10))
