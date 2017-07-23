module Zwerg.Component.TileMap (
    TileMap,
    tileUUIDatPosition
    ) where

import Zwerg.Prelude
import Zwerg.Util
import Zwerg.Component.Position

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M

newtype TileMap = MkTileMap (Map Position UUID)
    deriving (Eq, Show, Generic)

instance ZWrapped TileMap (Map Position UUID) where
    unwrap (MkTileMap tm) = tm

instance Binary TileMap

instance ZConstructable TileMap [(Position,UUID)] where
  zConstruct tl = if length tl /= (mapWidthINT * mapHeightINT)
      then $(throw) EngineFatal "Attempted to construct a TileMap with number of tiles not equal to mapWidth * mapHeight"
      else return . MkTileMap $ M.fromList tl

tileUUIDatPosition :: (MonadError ZError m)
                   => Position
                   -> TileMap
                   -> m UUID
tileUUIDatPosition pos (MkTileMap tm) =
  let maybeUUID = M.lookup pos tm
  in fromJustErrM maybeUUID $
       ZError __FILE__ __LINE__ EngineFatal
       "Attempted to find tile UUID at non-existent position"



