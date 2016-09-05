module Zwerg.Generator.Level (
    module EXPORTED,
    genLevelSkeleton,
    ) where

import Zwerg.Generator            as EXPORTED
import Zwerg.Component.EntityType as EXPORTED
import Zwerg.Component.Position   as EXPORTED
import Zwerg.Component.TileType   as EXPORTED
import Zwerg.Component.Tiles      as EXPORTED
import Zwerg.Const                as EXPORTED

import qualified Zwerg.Data.UUIDSet as US (empty)

import Control.Lens (use)
import Control.Monad (forM_)

genLevelSkeleton :: Generator ()
genLevelSkeleton =
    forM_ [(x,y) | x <- [0..mapWidth-1], y <- [0..mapHeight-1]] $ \(x',y') -> do
        uuid <- getNewUUID
        addComp uuid entityType Tile
        addComp uuid tileType Void
        addComp uuid position $ mkPosition (x',y')
        addComp uuid occupants US.empty
        addComp uuid blocked True
        addComp uuid needsRedraw True
        use targetUUID >>= addComp uuid level


