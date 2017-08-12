module Zwerg.Geometry.FOV (getFOV) where

import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Data.Position
-- import Zwerg.Geometry
-- import Zwerg.Util
-- import qualified Data.List as L (nub, concat)

type BlockedMap = GridMap Bool

getFOV :: Position -> Int -> BlockedMap -> [Position]
getFOV = getFOVLazy

getFOVLazy :: Position -> Int -> BlockedMap -> [Position]
getFOVLazy pos fov _ = filter inPlayerSightRange $ catMaybes $ map wrap $ [ (x,y) | x <- [0..mapWidthINT-1], y <- [0..mapHeightINT-1] ]
    where inPlayerSightRange p = round (distance Euclidean p pos) < fov


-- pureGetFOVRays :: Position -> Int -> BlockedMap -> [Position]
-- pureGetFOVRays pos fov blockedMap = visiblePos
--     where fovEdges = circle (unwrap pos) (2 * fov)
--           linesToFovEdges = map (tail . line (unwrap pos)) fovEdges
--           notBlocked is = case (wrap is) of
--                             Just p -> (not $ zAt blockedMap p) && (round (distance Euclidean pos p) < fov)
--                             Nothing -> False
--           unbrokenLines = map (takeUntil notBlocked) linesToFovEdges
--           visiblePos = pos : (map unsafeWrap $ L.nub $ L.concat unbrokenLines)

