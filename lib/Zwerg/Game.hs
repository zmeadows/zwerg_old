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
import Zwerg.Prelude
import Zwerg.Random.RanGen
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Control.Monad.Random (runRandT, RandT, MonadRandom)

data GameState = GameState
  { _gsComponents :: Components
  , _gsLog :: Log
  , _gsPortal :: Portal
  , _gsEventQueue :: ZwergEventQueue
  }

makeClassy ''GameState

emptyGameState :: GameState
emptyGameState =
  GameState
  { _gsComponents = emptyComponents
  , _gsLog = emptyLog
  , _gsPortal = [initMainMenu]
  , _gsEventQueue = zEmpty
  }

instance HasComponents GameState where
  components = gsComponents

instance HasLog GameState where
  userLog = gsLog

instance HasPortal GameState where
  portal = gsPortal

instance HasZwergEventQueue GameState where
  eventQueue = gsEventQueue

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

postEventHook :: Game ()
postEventHook = do
  newPort:_ <- use portal
  case newPort of
    MainScreen _ -> do
      ts <- use (ticks . uuidMap)
      (minTick, uuids) <- getMinimumUUIDs ts
      (ticks . uuidMap) %= fmap (\x -> max (x - minTick) 0)
      if | (not $ elem playerUUID uuids) ->
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
  setComp playerUUID ticks 50
  postEventHook

processUserInput' :: Portal -> KeyCode -> Game ()
processUserInput' (MainMenu m:ps) (KeyChar 'j') =
  portal .= (MainMenu $ next m) : ps
processUserInput' (MainMenu m:ps) (KeyChar 'k') = portal .= [MainMenu $ prev m]
processUserInput' (MainMenu m:ps) Return =
  case (view label $ focus m) of
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
processUserInput' (MainScreen _:_) (KeyChar 'h') =
  processPlayerDirectionInput West
processUserInput' (MainScreen _:_) (KeyChar 'j') =
  processPlayerDirectionInput South
processUserInput' (MainScreen _:_) (KeyChar 'k') =
  processPlayerDirectionInput North
processUserInput' (MainScreen _:_) (KeyChar 'l') =
  processPlayerDirectionInput East
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
  let uuid = ed ^. moverUUID
      dir = ed ^. direction
  oldPosition <- demandComp position uuid
  case movePosDir dir oldPosition
    -- TODO: log message, player tried to move off map
        of
    Nothing -> return ()
    Just newPos ->
      pushEventM $ MoveEntityEvent $ MoveEntityEventData playerUUID newPos
processEvent (MoveEntityEvent ed) = do
  let uuid = ed ^. moverUUID
      newPos = ed ^. newPosition
  oldPos <- demandComp position uuid
  levelUUID <- demandComp level uuid
  levelTiles <- demandComp tileMap levelUUID
  newTileUUID <- TM.tileUUIDatPosition newPos levelTiles
  newTileBlocked <- readC $ tileBlocksPassage newTileUUID
  when (newTileBlocked && uuid /= playerUUID) $
    throwError $
    ZError
      __FILE__
      __LINE__
      Fatal
      "NPC Entity attempted to move to blocked tile"
  when (newTileBlocked && uuid == playerUUID) $
    throwError $
    ZError
      __FILE__
      __LINE__
      Warning
      "Player attempted to move to a blocked tile"
  oldTileUUID <- TM.tileUUIDatPosition oldPos levelTiles
  transferOccupant uuid oldTileUUID newTileUUID
  setComp uuid position newPos
processEvent (WeaponAttackAttemptEvent ed) =
  pushEventM $ DeathEvent $ DeathEventData $ ed ^. defenderUUID
processEvent (DeathEvent ed) = eraseEntity $ ed ^. dyingUUID
processEvent _ = return ()

getGlyphMapUpdates :: MonadCompState GlyphMap
getGlyphMapUpdates = do
  tilesWithUpdatedNeeded <- readC $ getVisibleTiles playerUUID
  updatedGlyphs <-
    forM (zToList tilesWithUpdatedNeeded) $ \tileUUID -> do
      pos <- demandComp position tileUUID
      occUUID <- readC $ getPrimaryOccupant tileUUID
      gly <- demandComp glyph occUUID
      return (pos, (gly, True))
  return $ mkGlyphMap updatedGlyphs

processPlayerDirectionInput :: Direction -> Game ()
processPlayerDirectionInput dir = do
  case dir of
    North -> pushLogMsgM "player moved North"
    South -> pushLogMsgM "player moved South"
    East -> pushLogMsgM "player moved East"
    West -> pushLogMsgM "player moved West"
    _ -> return ()
  let processPlayerMove =
        pushEventM $
        MoveEntityDirectionEvent $ MoveEntityDirectionEventData playerUUID dir
      processPlayerAttack attackedUUID = do
        pushEventM $
          WeaponAttackAttemptEvent $
          WeaponAttackAttemptEventData playerUUID attackedUUID
  attackedEntity <- readC $ getPlayerAdjacentEnemy dir
  maybe processPlayerMove processPlayerAttack attackedEntity
