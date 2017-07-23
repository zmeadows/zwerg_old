module Zwerg.Game where

import Zwerg.Component
import Zwerg.Component.Position
import qualified Zwerg.Component.TileMap as TM
import Zwerg.Data.UUIDMap
import Zwerg.Entity
import Zwerg.Entity.AI
import Zwerg.Event
import Zwerg.Generator
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Player.TestPlayer
import Zwerg.Log
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Control.Monad.Random
       (runRandT, RandT, MonadRandom, getRandomR)

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

generateGame :: Game ()
generateGame = do
  lid <- generate testSquareGenerator
  generate $ testPlayerGenerator lid

processNonPlayerEvents :: Game ()
processNonPlayerEvents = do
  newPort:_ <- use portal
  case newPort of
    MainScreen _ -> do
      ts <- use (ticks . uuidMap)
      (minTick, uuids) <- getMinimumUUIDs ts
      (ticks . uuidMap) %= fmap (\x -> max (x - minTick) 0)
      if | notElem playerUUID uuids ->
           forM_ uuids $ \i -> do
             runAI i
             processEvents
             setComp i ticks 100
         | otherwise -> return ()
      updateGlyphMap
    _ -> return ()

processUserInput :: KeyCode -> Game ()
processUserInput k = do
  p <- use portal
  processUserInput' p k
  processEvents
  resetTicks playerUUID
  processNonPlayerEvents

processUserInput' :: Portal -> KeyCode -> Game ()

processUserInput' (MainMenu m:ps) (KeyChar 'j') = portal .= (MainMenu $ next m) : ps

processUserInput' (MainMenu m:_) (KeyChar 'k') = portal .= [MainMenu $ prev m]

processUserInput' (MainMenu m:_) Return =
  case view label $ focus m of
    "new game" -> do
      generateGame
      let xs = [0 .. mapWidthINT - 1]
          ys = [0 .. mapHeightINT - 1]
          emptyGlyph = Glyph ' ' Black0 Black0 (Just Black0) (Just Black0)
      ts <- mapM zConstruct [(x, y) | x <- xs, y <- ys]
      portal .=
        [MainScreen $ mkGlyphMap $ map (\p -> (p, (emptyGlyph, False))) ts]
    "exit" -> portal .= [ExitScreen]
    _ -> return ()

processUserInput' (MainScreen _:_) (KeyChar 'h') = processPlayerDirectionInput West

processUserInput' (MainScreen _:_) (KeyChar 'j') = processPlayerDirectionInput South

processUserInput' (MainScreen _:_) (KeyChar 'k') = processPlayerDirectionInput North

processUserInput' (MainScreen _:_) (KeyChar 'l') = processPlayerDirectionInput East

processUserInput' _ _ = return ()

updateGlyphMap :: Game ()
updateGlyphMap = do
  p:ps <- use portal
  case p of
    MainScreen gm -> do
      updatedGlyphs <- getGlyphMapUpdates
      portal .= (MainScreen $ mergeGlyphMaps updatedGlyphs gm) : ps
    _ -> return ()

processEvents :: Game ()
processEvents =
  whenJustM popEvent $ \nextEvent -> do
    processEvent nextEvent
    processEvents

processEvent :: ZwergEvent -> Game ()

processEvent (MoveEntityDirectionEvent ed) = do
  oldPosition <- position <@> (ed ^. moverUUID)
  case movePosDir (ed ^. direction) oldPosition of
    Nothing -> return () -- TODO: log message, player tried to move off map
    Just newPos -> $(newEvent "MoveEntity") playerUUID newPos

processEvent (MoveEntityEvent ed) = do
  oldPos <- position <@> (ed ^. moverUUID)
  levelTiles <- level <@> (ed ^. moverUUID) >>= (<@>) tileMap
  newTileUUID <- TM.tileUUIDatPosition (ed ^. newPosition) levelTiles
  newTileBlocked <- readC $ tileBlocksPassage newTileUUID
  when (newTileBlocked && (ed ^. moverUUID) /= playerUUID) $
    $(throw) EngineFatal "NPC Entity attempted to move to blocked tile"
  when (newTileBlocked && (ed ^. moverUUID) == playerUUID) $
    $(throw) PlayerWarning "Player attempted to move to a blocked tile"
  oldTileUUID <- TM.tileUUIDatPosition oldPos levelTiles
  transferOccupant (ed ^. moverUUID) oldTileUUID newTileUUID
  setComp (ed ^. moverUUID) position (ed ^. newPosition)

processEvent (WeaponAttackAttemptEvent ed)
  -- TODO: explicitely calculate hit probability
  -- TODO: Factor some of this out
 = do
  r <- getRandomR ((0.0, 1.0) :: (Double, Double))
  when (r < 0.5) $ do
    weaponUUID' <- readC $ getEquippedWeapon $ ed ^. attackerUUID
    whenJust weaponUUID' $ \weaponUUID -> do
      chain <- damageChain <@> weaponUUID
      forM_ chain $ \damageData -> do
        targetedUUIDs <-
          readC $
          getTargetedUUIDs (damageData ^. targetType) (ed ^. defenderUUID)
        forM_ targetedUUIDs $ \targetUUID ->
          pushEventM $
            IncomingDamageEvent $
            IncomingDamageEventData
              (ed ^. attackerUUID)
              targetUUID
              (damageData ^. attribute)
              (damageData ^. distribution)

processEvent (WeaponAttackHitEvent _) = return ()

processEvent (DeathEvent ed) = eraseEntity $ ed ^. dyingUUID

processEvent (IncomingDamageEvent ed) = do
  damageDone <- round <$> sample (ed ^. damageDistribution)
  $(newEvent "OutgoingDamage") (ed ^. attackerUUID) (ed ^. defenderUUID) damageDone

processEvent (OutgoingDamageEvent ed) = do
  modComp (ed ^. defenderUUID) hp (adjustHP $ subtract $ ed ^. damageAmount)
  newHP <- hp <@> (ed ^. defenderUUID)
  when (fst (unwrap newHP) == 0) $ eraseEntity $ ed ^. defenderUUID

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
processPlayerDirectionInput dir = do
  let processPlayerMove = do
        case dir of
          North -> pushLogMsgM "player moved North"
          South -> pushLogMsgM "player moved South"
          East  -> pushLogMsgM "player moved East"
          West  -> pushLogMsgM "player moved West"
          _     -> return ()
        pushEventM $
          MoveEntityDirectionEvent $ MoveEntityDirectionEventData playerUUID dir
        -- $(newEvent "MoveEntity") playerUUID dir
      processPlayerAttack attackedUUID =
        pushEventM $
        WeaponAttackAttemptEvent $
        WeaponAttackAttemptEventData playerUUID attackedUUID
  attackedEntity <- readC $ getPlayerAdjacentEnemy dir
  maybe processPlayerMove processPlayerAttack attackedEntity
