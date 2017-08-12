module Zwerg.Generator.Verify (verifyAndReturn) where

import Zwerg.Generator

verifyComponent :: Component a -> UUID -> MonadCompRead ()
verifyComponent !comp !uuid =
  whenM (not <$> canViewComp uuid comp) $ do
    cn <- view (comp . _1)
    debug $! "VERIFICATION FAILURE: " <> cn <> " " <> show uuid

verifyAndReturn :: UUID -> Generator' UUID
verifyAndReturn entityUUID = do
  etype <- entityType <@> entityUUID
  readC $ verifyAndReturn' entityUUID etype
  return entityUUID

--TODO: continually expand this as new components are added
--and new features are added to the game
verifyAndReturn' :: UUID -> EntityType -> MonadCompRead ()
verifyAndReturn' uuid Enemy = do
    verifyComponent name uuid
    verifyComponent occupants uuid
    verifyComponent needsRedraw uuid
    verifyComponent itemType uuid
  -- $(hasAll "uuid"
  --  [ "name" , "description" , "species"
  --  , "glyph" , "hp" , "entityType"
  --  , "stats" , "aiType" , "viewRange"
  --  ]
  -- )

verifyAndReturn' _ Level = return ()
    -- $(hasAll "uuid"
    -- $([ "name" , "description", "entityType" ]
   -- $(
   --TODO: loop over entities on level and do extra verification
   -- for example, require all Enemies in level to have position, tileOn, etc.

verifyAndReturn' _ Tile = return ()

verifyAndReturn' _ Item = return ()

verifyAndReturn' _ _ = return ()

