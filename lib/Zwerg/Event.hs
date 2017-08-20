module Zwerg.Event where

import Zwerg.Prelude
import Zwerg.Data.Damage
import Zwerg.Data.Position
import Zwerg.Random.Distribution

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data IncomingDamageEventData = IncomingDamageEventData
    { incDamAttackerUUID :: UUID
    , incDamDefenderUUID :: UUID
    , incDamAttribute    :: DamageAttribute
    , incDamDistribution :: Distribution }
  deriving stock Generic
  deriving anyclass Binary

data OutgoingDamageEventData = OutgoingDamageEventData
    { outDamAttackerUUID :: UUID
    , outDamDefenderUUID :: UUID
    , outDamDamageAmount :: Int }
  deriving stock Generic
  deriving anyclass Binary

data WeaponAttackAttemptEventData = WeaponAttackAttemptEventData
    { weapAtkAttempAttackerUUID :: UUID
    , weapAtkAttempDefenderUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data WeaponAttackHitEventData = WeaponAttackHitEventData
    { weapAtkHitAttackerUUID :: UUID
    , weapAtkHitDefenderUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data WeaponAttackMissEventData = WeaponAttackMissEventData
    { weapAtkMissAttackerUUID :: UUID
    , weapAtkMissDefenderUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data DeathEventData = DeathEventData
    { dyingUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data MoveEntityDirectionEventData = MoveEntityDirectionEventData
    { moveEntityDirMoverUUID :: UUID
    , moveEntityDirDirection :: Direction }
  deriving stock Generic
  deriving anyclass Binary

data MoveEntityEventData = MoveEntityEventData
    { moveEntityMoverUUID   :: UUID
    , moveEntityNewPosition :: Position }
  deriving stock Generic
  deriving anyclass Binary

data EntityLeftTileEventData = EntityLeftTileEventData
    { entityLeftTileLeaverUUID   :: UUID
    , entityLeftTileLeftTileUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data EntityReachedTileEventData = EntityReachedTileEventData
    { entityReachedTileReacherUUID     :: UUID
    , entityReachedTileReachedTileUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data EntityDroppedItemEventData = EntityDroppedItemEventData
    { entityDroppedItemDropperUUID :: UUID
    , entityDroppedItemDroppedUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data EntityPickedUpItemEventData = EntityPickedUpItemEventData
    { entityPickedUpItemPickerUUID :: UUID
    , entityPickedUpItemPickedUUID :: UUID }
  deriving stock Generic
  deriving anyclass Binary

data ZwergEvent
    = IncomingDamageEvent IncomingDamageEventData
    | OutgoingDamageEvent OutgoingDamageEventData
    | WeaponAttackAttemptEvent WeaponAttackAttemptEventData
    | WeaponAttackHitEvent WeaponAttackHitEventData
    | WeaponAttackMissEvent WeaponAttackMissEventData
    | DeathEvent DeathEventData
    | MoveEntityEvent MoveEntityEventData
    | MoveEntityDirectionEvent MoveEntityDirectionEventData
    | EntityLeftTileEvent EntityLeftTileEventData
    | EntityReachedTileEvent EntityReachedTileEventData
    | EntityDroppedItemEvent EntityDroppedItemEventData
    | EntityPickedUpItemEvent EntityPickedUpItemEventData
  deriving stock Generic
  deriving anyclass Binary

newEvent :: Language.Haskell.TH.Syntax.Quasi m => Text -> m Exp
newEvent "IncomingDamage"      = runQ [| \a b c d -> pushEventM $ IncomingDamageEvent $ IncomingDamageEventData a b c d|]
newEvent "OutgoingDamage"      = runQ [| \a b c -> pushEventM $ OutgoingDamageEvent $ OutgoingDamageEventData a b c|]
newEvent "WeaponAttackAttempt" = runQ [| \a b -> pushEventM $ WeaponAttackAttemptEvent $ WeaponAttackAttemptEventData a b|]
newEvent "WeaponAttackHit"     = runQ [| \a b -> pushEventM $ WeaponAttackHitEvent $ WeaponAttackHitEventData a b|]
newEvent "WeaponAttackMiss"    = runQ [| \a b -> pushEventM $ WeaponAttackMissEvent $ WeaponAttackMissEventData a b|]
newEvent "MoveEntity"          = runQ [| \a b -> pushEventM $ MoveEntityEvent $ MoveEntityEventData a b|]
newEvent "MoveEntityDirection" = runQ [| \a b -> pushEventM $ MoveEntityDirectionEvent $ MoveEntityDirectionEventData a b|]
newEvent "EntityLeftTile"      = runQ [| \a b -> pushEventM $ EntityLeftTileEvent $ EntityLeftTileEventData a b|]
newEvent "EntityReachedTile"   = runQ [| \a b -> pushEventM $ EntityReachedTileEvent $ EntityReachedTileEventData a b|]
newEvent "EntityDroppedItem"   = runQ [| \a b -> pushEventM $ EntityDroppedItemEvent $ EntityDroppedItemEventData a b|]
newEvent "EntityPickedUpItem"  = runQ [| \a b -> pushEventM $ EntityPickedUpItemEvent $ EntityPickedUpItemEventData a b|]
newEvent _                     = runQ [|"INVALID EVENT TYPE PASSED TO TEMPLATE HASKELL FUNCTION 'newEvent'"|]
