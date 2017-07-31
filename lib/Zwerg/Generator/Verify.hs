module Zwerg.Generator.Verify (verifyAndReturn) where

import Zwerg.Generator

verifyAndReturn :: UUID -> Generator' UUID
verifyAndReturn entityUUID = do
  etype <- entityType <@> entityUUID
  readC $ verifyAndReturn' entityUUID etype
  return entityUUID

--TODO: continually expand this as new components are added
--and new features are added to the game
verifyAndReturn' :: UUID -> EntityType -> MonadCompRead ()
verifyAndReturn' uuid Enemy =
  $(hasAll "uuid"
    [ "name" , "description" , "species"
    , "glyph" , "hp" , "entityType"
    , "stats" , "aiType" , "viewRange"
    ]
   )

verifyAndReturn' uuid Level =
  $(hasAll "uuid"
    [ "name" , "description", "entityType" ]
   )
   --TODO: loop over entities on level and do extra verification
   -- for example, require all Enemies in level to have position, tileOn, etc.

verifyAndReturn' _ Tile = return ()

verifyAndReturn' _ Item = return ()

verifyAndReturn' _ _ = return ()
