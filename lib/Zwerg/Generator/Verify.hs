module Zwerg.Generator.Verify (verifyAndReturn) where

import Zwerg.Generator

verifyAndReturn :: UUID -> Generator' UUID
verifyAndReturn entityUUID = do
  etype <- entityType <@> entityUUID
  readC $ verifyAndReturn' entityUUID etype
  return entityUUID

verifyAndReturn' :: UUID -> EntityType -> MonadCompRead ()
verifyAndReturn' enemyUUID Enemy =
  $(hasAll "enemyUUID"
    [ "name" , "description" , "species"
    , "glyph" , "hp" , "entityType"
    , "stats" , "aiType" , "viewRange"
    ]
   )

verifyAndReturn' _ Level = return ()

verifyAndReturn' _ Tile = return ()

verifyAndReturn' _ Item = return ()

verifyAndReturn' _ _ = return ()
