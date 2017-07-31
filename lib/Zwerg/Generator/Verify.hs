module Zwerg.Generator.Verify (verifyAndReturn) where

import Zwerg.Generator

--TODO: write custom template haskell function to do checks
-- so that a more informative error message will be shown

verifyAndReturn :: UUID -> Generator' UUID
verifyAndReturn entityUUID = do
  etype <- entityType <@> entityUUID
  readC $ verifyAndReturn' entityUUID etype
  return entityUUID

verifyAndReturn' :: UUID -> EntityType -> MonadCompRead ()
verifyAndReturn' enemyUUID Enemy = do
  name <~!> enemyUUID
  description <~!> enemyUUID
  species <~!> enemyUUID
  glyph <~!> enemyUUID
  hp <~!> enemyUUID
  ticks <~!> enemyUUID
  stats <~!> enemyUUID
  blocksPassage <~!> enemyUUID
  blocksVision <~!> enemyUUID

verifyAndReturn' _ Level = return ()

verifyAndReturn' _ Tile = return ()

verifyAndReturn' _ Item = return ()

verifyAndReturn' _ _ = return ()
