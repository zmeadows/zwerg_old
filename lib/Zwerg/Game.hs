module Zwerg.Game where

import Zwerg.Component
import Zwerg.Data.Position
import Zwerg.Data.UUIDMap
import Zwerg.Entity
import Zwerg.Entity.AI
import Zwerg.Event.Queue
import Zwerg.Generator
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Player.TestPlayer
import Zwerg.Log
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port

import qualified Data.Text as T (concat)

import Control.Monad.Random (runRandT, RandT, MonadRandom, getRandomR)



data GameState = GameState
  { _gsComponents :: Components
  , _gsLog        :: Log
  , _gsPortal     :: Portal
  , _gsEventQueue :: ZwergEventQueue
  } deriving (Show, Eq, Generic)
makeClassy ''GameState

instance Binary GameState

instance HasComponents GameState where
  components = gsComponents
instance HasLog GameState where
  userLog = gsLog
instance HasPortal GameState where
  portal = gsPortal
instance HasZwergEventQueue GameState where
  eventQueue = gsEventQueue

emptyGameState :: GameState
emptyGameState =
  GameState
  { _gsComponents = emptyComponents
  , _gsLog        = emptyLog
  , _gsPortal     = [initMainMenu]
  , _gsEventQueue = zEmpty
  }

-- Highest level purely-functional context which encapsulates
-- all game logic/state/error handling
newtype Game a =
  Game (ExceptT ZError (RandT RanGen (State GameState)) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GameState
           , MonadError ZError
           , MonadRandom
           )

runGame :: Game () -> RanGen -> GameState -> (GameState, Maybe ZError, RanGen)
runGame (Game a) gen st =
  let ((e, gen'), st') = runState (runRandT (runExceptT a) gen) st
  in case e of
       Left err -> (st', Just err, gen')
       Right () -> (st', Nothing, gen')

-- for now just generates the test level
generateGame :: Game ()
generateGame = testSquareGenerator >>= testPlayerGenerator

-- after we process a player tick, go through all other entities
-- and process their ticks, until the player is ready to tick again
processNonPlayerEvents :: Game ()
processNonPlayerEvents = do
  use portal >>= \case
    MainScreen _ : _ -> do
      ts <- use (ticks . uuidMap)
      (minTick, uuids) <- getMinimumUUIDs ts
      (ticks . uuidMap) %= fmap (\x -> max (x - minTick) 0)
      if | notElem playerUUID uuids ->
           forM_ uuids $ \i -> do
             runAI i
             processEvents
             -- TODO: set ticks according to DEX + other things
             setComp i ticks 100
         | otherwise -> return $! ()
      updateGlyphMap
    _ -> return $! ()

processUserInput :: KeyCode -> Game ()
processUserInput k = do
  p <- use portal
  processUserInput' p k
  eq <- use eventQueue
  -- only move on to process other entity ticks
  -- if player actually did something
  when (not $ zIsNull eq) $ do
    processEvents
    resetTicks playerUUID
    processNonPlayerEvents

processUserInput' :: Portal -> KeyCode -> Game ()

processUserInput' (MainMenu m : ps) (KeyChar 'j') = portal .= (MainMenu $ next m) : ps
processUserInput' (MainMenu m : ps) (KeyChar 'k') = portal .= (MainMenu $ prev m) : ps

processUserInput' (MainMenu m:_) Return =
  case focus m ^. label of
    "new game" -> do
      generateGame
      gm <- blankGlyphMap
      portal .= [MainScreen gm]
      updateGlyphMap
    "exit" -> portal .= [ExitScreen]
    _ -> return $! ()

processUserInput' (MainScreen _:_) (KeyChar 'h') = processPlayerDirectionInput West
processUserInput' (MainScreen _:_) (KeyChar 'j') = processPlayerDirectionInput South
processUserInput' (MainScreen _:_) (KeyChar 'k') = processPlayerDirectionInput North
processUserInput' (MainScreen _:_) (KeyChar 'l') = processPlayerDirectionInput East
processUserInput' (MainScreen _:_) (LeftArrow)   = processPlayerDirectionInput West
processUserInput' (MainScreen _:_) (UpArrow)     = processPlayerDirectionInput South
processUserInput' (MainScreen _:_) (DownArrow)   = processPlayerDirectionInput North
processUserInput' (MainScreen _:_) (RightArrow)  = processPlayerDirectionInput East

processUserInput' p@(MainScreen _:_) (KeyChar 'i') = do
  inv <- zToList <$> inventory <@> playerUUID
  menuItems <- forM inv $ \uuid -> do
    d <- description <@> uuid
    n <- name <@> uuid
    return $! (n, InventoryMenuItem uuid d)
  case menuItems of
    [] -> pushLogMsgM "You don't have any items to look at."
    i:is -> portal .= (ViewInventory $ makeMenu $ i :| is) : p

processUserInput' (ViewInventory _ : ps) (KeyChar 'i') = portal .= ps

processUserInput' p@(MainScreen _ : _) (KeyChar 'x') = do
  playerPos <- position <@> playerUUID
  portal .= ExamineTiles playerPos : p

processUserInput' (ExamineTiles _ : ps) (KeyChar 'x') = portal .= ps

processUserInput' (ExamineTiles pos : ps) (KeyChar 'h') = do
  case movePosDir West pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return $! ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'j') = do
  case movePosDir South pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return $! ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'k') = do
  case movePosDir North pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return $! ()

processUserInput' (ExamineTiles pos : ps) (KeyChar 'l') = do
  case movePosDir East pos of
    Just newPos -> portal .= ExamineTiles newPos : ps
    Nothing -> return $! ()

processUserInput' _ _ = return $! ()

updateGlyphMap :: Game ()
updateGlyphMap = do
  use portal >>= \case
    MainScreen gm : ps -> do
      updatedGlyphs <- getGlyphMapUpdates
      portal .= (MainScreen $ mergeGlyphMaps updatedGlyphs gm) : ps
    _ -> return $! ()

processEvents :: Game ()
processEvents =
  whenJustM popEvent $! \nextEvent -> do
    processEvent nextEvent
    processEvents

processEvent :: ZwergEvent -> Game ()

processEvent (MoveEntityDirectionEvent ed) = do
  oldPosition <- position <@> (ed ^. moverUUID)
  case movePosDir (ed ^. direction) oldPosition of
    Nothing -> pushLogMsgM "You cannot move into the void."
    Just newPos -> $(newEvent "MoveEntity") (ed ^. moverUUID) newPos

processEvent (MoveEntityEvent ed) = do
  oldPos <- position <@> (ed ^. moverUUID)
  levelTiles <- level <@> (ed ^. moverUUID) >>= (<@>) tileMap
  let newTileUUID = atPos (ed ^. newPosition) levelTiles
  newTileBlocked <- readC $ tileBlocksPassage newTileUUID

  if newTileBlocked
     then if (ed ^. moverUUID) /= playerUUID
             then $(throw) EngineFatal "NPC Entity attempted to move to blocked tile"
             else pushLogMsgM "You cannot move into a blocked tile."
     else do
       let oldTileUUID = atPos oldPos levelTiles
       transferOccupant (ed ^. moverUUID) (Just oldTileUUID) newTileUUID
       setComp (ed ^. moverUUID) position (ed ^. newPosition)

processEvent (WeaponAttackAttemptEvent ed) = do
  attDEX <- readC $ getStat DEX $ ed ^. attackerUUID
  defDEX <- readC $ getStat DEX $ ed ^. defenderUUID
  let prob = if attDEX > defDEX then 0.75 else 0.5 :: Double
  r <- getRandomR (0.0, 1.0)
  if (r < prob)
     then $(newEvent "WeaponAttackHit") (ed ^. attackerUUID) (ed ^. defenderUUID)
     else $(newEvent "WeaponAttackMiss") (ed ^. attackerUUID) (ed ^. defenderUUID)

processEvent (WeaponAttackHitEvent ed) = do
  readC (getEquippedWeapon $ ed ^. attackerUUID) >>= \case
    --TODO: decide how to handle unarmed attacks
    Nothing -> return $! ()
    Just weaponUUID -> do
      chain <- damageChain <@> weaponUUID
      forM_ chain $ \damageData -> do
        targetedUUIDs <- readC $ getTargetedUUIDs (damageData ^. targetType) (ed ^. defenderUUID)
        forM_ targetedUUIDs $ \targetUUID ->
          $(newEvent "IncomingDamage") (ed ^. attackerUUID)
                                       targetUUID
                                       (damageData ^. attribute)
                                       (damageData ^. distribution)

processEvent (WeaponAttackMissEvent _) = return $! ()

processEvent (DeathEvent ed) = eraseEntity $ ed ^. dyingUUID

processEvent (IncomingDamageEvent ed) = do
  --TODO: account for weaknesses in creatures and armor
    damageDone <- round <$> sample (ed ^. damageDistribution)
    $(newEvent "OutgoingDamage") (ed ^. attackerUUID) (ed ^. defenderUUID) damageDone

processEvent (OutgoingDamageEvent ed) = do
  stillAlive <- hasComp (ed ^. defenderUUID) hp
  when stillAlive $ do
    modComp (ed ^. defenderUUID) hp (adjustHP $ subtract $ ed ^. damageAmount)
    newHP <- hp <@> (ed ^. defenderUUID)
    when (ed ^. attackerUUID == playerUUID || ed ^. defenderUUID == playerUUID) $ do
      attName <- name <@> (ed ^. attackerUUID)
      defName <- name <@> (ed ^. defenderUUID)
      pushLogMsgM $ T.concat [attName, " hit ", defName, " for ", show $ ed ^. damageAmount, " damage."]

    when (fst (unwrap newHP) == 0) $
      if (ed ^. defenderUUID) == playerUUID
         then portal %= (DeathScreen "You died." :)
         else eraseEntity $ ed ^. defenderUUID

processEvent _ = return ()

-- FIXME: need to make distinction between visible/needsRedraw tiles...
-- FIXME: this should be MonadCompReader?
getGlyphMapUpdates :: MonadCompState GlyphMap
getGlyphMapUpdates = do
  visibleTiles <- readC $ getVisibleTiles playerUUID
  tilesWithUpdatedNeeded <- zFilterM (needsRedraw <@>) visibleTiles

  updatedGlyphs <-
    forM (zToList tilesWithUpdatedNeeded) $ \tileUUID -> do
      pos <- position <@> tileUUID
      occUUID <- readC $ getPrimaryOccupant tileUUID
      gly <- glyph <@> occUUID
      --setComp tileUUID needsRedraw False
      return (pos, (gly, True))

  return $ mkGlyphMap updatedGlyphs

processPlayerDirectionInput :: Direction -> Game ()
processPlayerDirectionInput dir =
  readC (getPlayerAdjacentEnemy dir) >>= \case
    Just attackedUUID -> $(newEvent "WeaponAttackAttempt") playerUUID attackedUUID
    Nothing -> $(newEvent "MoveEntityDirection") playerUUID dir
