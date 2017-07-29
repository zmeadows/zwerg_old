module Zwerg.Generator.Level
  ( module EXPORTED
  , levelSkeletonGenerator
  ) where

import Zwerg.Generator as EXPORTED

levelSkeletonGenerator :: Generator
levelSkeletonGenerator = do
  levelUUID <- popUUID
  emptyTileMap <- makeGridMapM $ \pos -> do
      uuid <- popUUID
      addComp uuid entityType Tile
      addComp uuid tileType Void
      addComp uuid position pos
      addComp uuid occupants zEmpty
      addComp uuid blocksPassage True
      addComp uuid needsRedraw True
      addComp uuid level levelUUID
      return uuid
  setComp levelUUID tileMap emptyTileMap
  setComp levelUUID tiles $ zFromList $ gridMapElems emptyTileMap
  setComp levelUUID entityType Level
  setComp levelUUID name "Test Square Level"
  return levelUUID
