module Zwerg.Geometry.FOV (getFOV) where

import Zwerg.Data.GridMap
import Zwerg.Data.Position

type BlockedMap = GridMap Bool
type VisibleMap = GridMap Bool

getFOV :: Position -> BlockedMap -> VisibleMap


-- import qualified Data.HashTable.ST.Basic as H
-- 
-- type BlockMap = H.BasicHashTable Int Bool
-- 
-- 
-- 
-- import Data.In
-- 
-- data FOVContext = FOVContext
--   { _fovRange  :: Double
--   , _tileMap   :: Map (Int, Int) (Bool, UUID)
--   , _playerPos :: (Int, Int)
--   }
-- 
-- runFOValg :: IntMap Bool
-- 
-- 
-- type FOVAlgorithm = StateT UUIDSet (Reader FOVContext) ()
-- 
-- makeTileMap :: MonadCompReader (Map (Int, Int) (Bool, UUID))
-- makeTileMap = return M.empty
-- 
-- runFOValg :: FOVAlgorithm -> MonadCompReader UUIDSet
-- runFOValg _ = return zEmpty
-- 
-- -- FOV --
-- simpleFOV :: FOVAlgorithm
-- simpleFOV = return ()
