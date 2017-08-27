module Zwerg.Game where
--TODO: export only what you want, and factor out processEvent into new module

import Zwerg.Component
import Zwerg.Data.Position
import Zwerg.Data.UUIDMap
import Zwerg.Entity
import Zwerg.Entity.AI
import Zwerg.Entity.Compare
import Zwerg.Event.Queue
import Zwerg.Generator
import Zwerg.Generator.Level
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Stairs
import Zwerg.Generator.Level.Cave
import Zwerg.Generator.Player.TestPlayer
import Zwerg.Log
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port

import Control.Monad.Random (runRandT, RandT, MonadRandom, getRandomR)

import Lens.Micro.Platform (makeClassy, (%=), use, _2, (.=))

data GameState = GameState
    { _gsComponents :: Components
    , _userLog      :: Log
    , _portal       :: Portal
    , _gsEventQueue :: ZwergEventQueue
    , _playerGoofed :: Bool
    }
  deriving stock Generic
  deriving anyclass Binary
makeClassy ''GameState

instance HasComponents GameState where
  components = gsComponents
instance HasZwergEventQueue GameState where
  eventQueue = gsEventQueue

pushLogMsgM :: Text -> Game ()
pushLogMsgM message = userLog %= pushLogMsg message

emptyGameState :: GameState
emptyGameState = GameState
  { _gsComponents = zDefault
  , _userLog      = zDefault
  , _portal       = [zDefault]
  , _gsEventQueue = zDefault
  , _playerGoofed = False
  }

-- Highest level purely-functional context which encapsulates all game logic/state
newtype Game' a = Game (RandT RanGen (State GameState) a)
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadState GameState
                     , MonadRandom
                     )

type Game a = HasCallStack => Game' a

runGame :: Game () -> RanGen -> GameState -> GameState
runGame (Game a) gen st = execState (runRandT a gen) st

-- TODO: factor out into Generator.World module
generateGame :: Game ()
generateGame = do
    caveUUID <- caveGenerator
    testUUID <- testSquareGenerator
    setZLevel caveUUID (unsafeWrap 0)
    setZLevel testUUID (unsafeWrap 1)
    buildRandomStairs testUUID caveUUID
    buildRandomStairs testUUID caveUUID
    buildRandomStairs testUUID caveUUID
    buildRandomStairs testUUID caveUUID
    testPlayerGenerator caveUUID

-- after we process a player tick, go through all other entities
-- and process their ticks, until the player is ready to tick again
processNonPlayerEvents :: Game ()
processNonPlayerEvents = do
  use portal >>= \case
    (MainScreen:_) -> do
        (minTick, uuids) <- getMinimumUUIDs <$> getCompUUIDMap ticks
        (ticks . _2) %= fmap (\x -> max (x - minTick) 0)
        if | notElem playerUUID uuids ->
               forM_ uuids $ \i -> do runAI i >> processEvents >> setComp i ticks 100
           | otherwise -> return ()
        updateGlyphMap
    _ -> return ()

processUserInput :: KeyCode -> Game ()
processUserInput k = do
  playerGoofed .= False
  p <- use portal
  processUserInput' p k
  playerGeneratedEvent <- (not . zIsNull) <$> use eventQueue
  playerScrewedUp <- use playerGoofed
  when (not playerScrewedUp && playerGeneratedEvent) $ do
    processEvents
    resetTicks playerUUID
    processNonPlayerEvents

processUserInput' :: Portal -> KeyCode -> Game ()

processUserInput' (MainMenu m : ps) (KeyChar 'j') = portal .= (MainMenu $ next m) : ps
processUserInput' (MainMenu m : ps) (KeyChar 'k') = portal .= (MainMenu $ prev m) : ps

processUserInput' (MainMenu m:_) Return =
    case label (focus m) of
        "new game" -> do
            generateGame
            portal .= [MainScreen]
            updateGlyphMap
        "exit" -> portal .= [ExitScreen]
        _ -> return ()

processUserInput' (MainScreen:_) (KeyChar 'h') = processPlayerDirectionInput $ Cardinal West
processUserInput' (MainScreen:_) (KeyChar 'j') = processPlayerDirectionInput $ Cardinal South
processUserInput' (MainScreen:_) (KeyChar 'k') = processPlayerDirectionInput $ Cardinal North
processUserInput' (MainScreen:_) (KeyChar 'l') = processPlayerDirectionInput $ Cardinal East
processUserInput' (MainScreen:_) (LeftArrow)   = processPlayerDirectionInput $ Cardinal West
processUserInput' (MainScreen:_) (UpArrow)     = processPlayerDirectionInput $ Cardinal South
processUserInput' (MainScreen:_) (DownArrow)   = processPlayerDirectionInput $ Cardinal North
processUserInput' (MainScreen:_) (RightArrow)  = processPlayerDirectionInput $ Cardinal East

processUserInput' (MainScreen:_) (KeyChar '>') = $(newEvent "EntityUseStairs") playerUUID Down
processUserInput' (MainScreen:_) (KeyChar '<') = $(newEvent "EntityUseStairs") playerUUID Up

processUserInput' p@(MainScreen:_) (KeyChar 'i') = do
  uuids <- unwrap <$> inventory <@> playerUUID
  names <- mapM (name <@>) uuids
  case zip names uuids of
    [] -> do
      pushLogMsgM "You don't have any items to look at."
      playerGoofed .= True
    i:is -> do
      portal .= (ViewInventory $ makeMenuGroupSelect $ i :| is) : p

processUserInput' (ViewInventory inv : ps) (KeyChar 'd') =
  portal .= (ViewInventory $ toggleFocus inv) : ps

processUserInput' (ViewInventory inv : ps) (KeyChar 'D') = do
    forM_ (getAllSelected inv) $ \droppedItemUUID ->
        $(newEvent "EntityDroppedItem") playerUUID droppedItemUUID
    portal .= ps

processUserInput' (ViewInventory _ : ps) (KeyChar 'i') = portal .= ps

processUserInput' (ViewInventory inv : ps) (KeyChar 'j') =
  portal .= (ViewInventory $ next inv) : ps

processUserInput' (ViewInventory inv : ps) (KeyChar 'k') =
  portal .= (ViewInventory $ prev inv) : ps

processUserInput' p@(MainScreen:_) (KeyChar 'x') = do
  playerPos <- position <@> playerUUID
  portal .= ExamineTiles playerPos : p

processUserInput' (ExamineTiles _ : ps) (KeyChar 'x') = portal .= ps

processUserInput' (ExamineTiles pos : ps) (KeyChar 'h') =
  case movePosDir (Cardinal West) pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'j') =
  case movePosDir (Cardinal South) pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'k') =
  case movePosDir (Cardinal North) pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'l') =
  case movePosDir (Cardinal East) pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return ()

processUserInput' _ _ = return ()

--TODO: pull GlyphMap from level.
updateGlyphMap :: Game ()
updateGlyphMap =
  use portal >>= \case
    MainScreen : _ -> do
        levelUUID <- level <@> playerUUID
        gm <- glyphMap <@> levelUUID
        updatedGlyphs <- readC getGlyphMapUpdates
        setComp levelUUID glyphMap $ mergeUpdates (fmap (markVisibility False) gm) updatedGlyphs
    _ -> return ()

processEvents :: Game ()
processEvents =
    whenJustM popEvent $ \nextEvent -> do
        processEvent nextEvent
        processEvents

processEvent :: ZwergEvent -> Game ()

processEvent (MoveEntityDirectionEvent MoveEntityDirectionEventData{..}) = do
  oldPosition <- position <@> moveEntityDirMoverUUID
  case movePosDir moveEntityDirDirection oldPosition of
    Nothing -> do
        -- TODO: check if non player entity and give error
      pushLogMsgM "You cannot move into the void."
      playerGoofed .= True
    Just newPos -> $(newEvent "MoveEntity") moveEntityDirMoverUUID newPos

processEvent (MoveEntityEvent MoveEntityEventData{..}) = do
  oldTileUUID <- tileOn <@> moveEntityMoverUUID
  levelTiles <- level <@> moveEntityMoverUUID >>= (<@>) tileMap
  let newTileUUID = zAt levelTiles moveEntityNewPosition
  newTileBlocked <- readC $ tileBlocksPassage newTileUUID

  if newTileBlocked
     then if moveEntityMoverUUID /= playerUUID
             then debug "NPC Entity attempted to move to blocked tile"
             else do
               pushLogMsgM "You cannot move into a blocked tile."
               playerGoofed .= True
     else do
       transferOccupant moveEntityMoverUUID (Just oldTileUUID) newTileUUID
       $(newEvent "EntityLeftTile") moveEntityMoverUUID oldTileUUID
       $(newEvent "EntityReachedTile") moveEntityMoverUUID newTileUUID

processEvent (EntityLeftTileEvent _) = return ()

processEvent (EntityReachedTileEvent _) = return ()

processEvent (WeaponAttackAttemptEvent WeaponAttackAttemptEventData{..}) = do
  attDEX <- readC $ getStat DEX $ weapAtkAttempAttackerUUID
  defDEX <- readC $ getStat DEX $ weapAtkAttempDefenderUUID
  let prob = if attDEX > defDEX then 0.75 else 0.5 :: Double
  r <- getRandomR (0.0, 1.0)
  if (r < prob)
     then $(newEvent "WeaponAttackHit") weapAtkAttempAttackerUUID weapAtkAttempDefenderUUID
     else $(newEvent "WeaponAttackMiss") weapAtkAttempAttackerUUID weapAtkAttempDefenderUUID

processEvent (WeaponAttackHitEvent WeaponAttackHitEventData{..}) = do
  readC (getEquippedWeapon weapAtkHitAttackerUUID) >>= \case
    --TODO: decide how to handle unarmed attacks
    Nothing -> return ()
    Just weaponUUID -> do
      chain <- damageChain <@> weaponUUID
      forM_ chain $ \damageData -> do
          targetedUUIDs <- readC $ getTargetedUUIDs (ddTargetType damageData) weapAtkHitDefenderUUID
          forM_ targetedUUIDs $ \targetUUID ->
              $(newEvent "IncomingDamage")
                  weapAtkHitAttackerUUID
                  targetUUID
                  (ddAttribute damageData)
                  (ddDistribution damageData)

processEvent (EntityUseStairsEvent EntityUseStairsEventData{..}) = do
  tileUUID <- tileOn <@> entityUseStairsUUID
  tileType <@> tileUUID >>= \case
    Stairs dir -> do
         newTileUUID <- connectedTo <@> tileUUID
         newTileBlocked <- readC $ tileBlocksPassage newTileUUID
         let stairsGoExpectedDirection = dir == entityUseStairsUpOrDown
             isPlayer = entityUseStairsUUID == playerUUID
         if | newTileBlocked && not isPlayer ->
                  debug "NPC Entity attempted to move down stairs to blocked tile."
            | newTileBlocked && isPlayer -> do
                  pushLogMsgM "There's something down there blocking your way."
                  playerGoofed .= True
            | not stairsGoExpectedDirection && not isPlayer ->
                  debug "NPC Entity attempted to move down up-going stairs!"
            | not stairsGoExpectedDirection && isPlayer -> do
                  pushLogMsgM "You can't go down the up stairs."
                  playerGoofed .= True
            | otherwise -> do
                transferOccupant entityUseStairsUUID (Just tileUUID) newTileUUID
                $(newEvent "EntityLeftTile") entityUseStairsUUID tileUUID
                $(newEvent "EntityReachedTile") entityUseStairsUUID newTileUUID
    _ -> debug "Entity attempted to use stairs while not on staircase."

processEvent (WeaponAttackMissEvent _) = return ()

processEvent (DeathEvent DeathEventData{..}) = eraseEntity dyingUUID

processEvent (IncomingDamageEvent IncomingDamageEventData{..}) = do
  --TODO: account for weaknesses in creatures and armor
    damageDone <- round <$> sample incDamDistribution
    $(newEvent "OutgoingDamage") incDamAttackerUUID incDamDefenderUUID damageDone

processEvent (OutgoingDamageEvent OutgoingDamageEventData{..}) = do
    whenM (hasComp outDamDefenderUUID hp) $ do
        modComp outDamDefenderUUID hp (adjustHP $ subtract $ outDamDamageAmount)
        newHP <- hp <@> outDamDefenderUUID

        when (outDamAttackerUUID == playerUUID || outDamDefenderUUID == playerUUID) $ do
            (attName, defName) <- name <@@!> (outDamAttackerUUID, outDamDefenderUUID)
            pushLogMsgM $ attName <> " hit " <> defName <> " for " <> (show $ outDamDamageAmount) <> " damage."

        when (fst (unwrap newHP) == 0) $
          if outDamDefenderUUID == playerUUID
             then portal %= (DeathScreen "You died." :)
             else eraseEntity outDamDefenderUUID

processEvent (EntityDroppedItemEvent EntityDroppedItemEventData{..}) = do
    tileUUID <- tileOn <@> entityDroppedItemDropperUUID
    modComp entityDroppedItemDropperUUID inventory
      $ zDelete entityDroppedItemDroppedUUID
    modComp tileUUID occupants $ zAdd entityDroppedItemDroppedUUID

processEvent _ = return ()

getGlyphMapUpdates :: MonadCompRead [(Position,GlyphMapCell)]
getGlyphMapUpdates = do
  visibleTiles <- getVisibleTiles playerUUID
  forM visibleTiles $ \tileUUID -> do
    pos <- position <~> tileUUID
    primaryGlyph <- getPrimaryOccupant tileUUID >>= (<~>) glyph

    let go bgGlyph = return (pos, GlyphMapCell True primaryGlyph bgGlyph)

    getPrimaryStationaryOccupant tileUUID >>= (<~>) glyph >>= \case
      PartialGlyph stationaryChar stationaryFG -> do
        glyph <~> tileUUID >>= \case
          FullGlyph _ _ tileBG -> go $ FullGlyph stationaryChar stationaryFG tileBG
          _ -> go zDefault
      stationaryGlyph@(FullGlyph _ _ _) -> go stationaryGlyph
      _ -> go zDefault

processPlayerDirectionInput :: Direction -> Game ()
processPlayerDirectionInput dir = getPlayerAdjacentEnemy >>= \case
      Just attackedUUID -> $(newEvent "WeaponAttackAttempt") playerUUID attackedUUID
      Nothing -> $(newEvent "MoveEntityDirection") playerUUID dir
  where getPlayerAdjacentEnemy = readC $ do
          attackedTileUUID <- tileOn <~> playerUUID >>= getAdjacentTileUUID dir
          case attackedTileUUID of
            Just attackedTileUUID' -> do
              unwrap <$> getOccupantsOfType attackedTileUUID' Enemy >>= \case
                [] -> return Nothing
                [x] -> return $ Just x
                xs -> do
                    debug "found multiple enemies on same tile"
                    return $ Just $ head xs
            Nothing -> return Nothing
