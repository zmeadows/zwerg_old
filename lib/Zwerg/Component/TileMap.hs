module Zwerg.Component.TileMap (
    TileMap,
    tileUUIDatPosition
    ) where

import Zwerg.Prelude
import Zwerg.Class
import Zwerg.Const
import Zwerg.Util
import Zwerg.Component.UUID
import Zwerg.Component.Position
import Zwerg.Data.Error
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Binary
import GHC.Generics (Generic)

newtype TileMap = MkTileMap (Map Position UUID)
    deriving (Eq, Show, Generic)

instance ZWrapped TileMap (Map Position UUID) where
    unwrap (MkTileMap tm) = tm

instance Binary TileMap

instance ZConstructable TileMap [(Position,UUID)] where
  zConstruct tl = if (length tl /= (round $ mapWidth * mapHeight))
      then throwError $ ZError __FILE__ __LINE__ Fatal
           "Attempted to construct a TileMap with number of tiles \
           not equal to mapWidth * mapHeight"
      else return . MkTileMap $ M.fromList tl

tileUUIDatPosition :: (MonadError ZError m)
                   => Position
                   -> TileMap
                   -> m UUID
tileUUIDatPosition pos (MkTileMap tm) =
  let maybeUUID = M.lookup pos tm
  in fromJustErrM maybeUUID $
       ZError __FILE__ __LINE__ Fatal
       "Attempted to find tile UUID at non-existent position"

  
  
