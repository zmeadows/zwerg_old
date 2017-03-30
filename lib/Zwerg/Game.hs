module Zwerg.Game where

import Zwerg.Component
import qualified Zwerg.Component.TileMap as TM
import qualified Zwerg.Data.Direction as Direction
import Zwerg.Data.Error (ZError)
import Zwerg.Data.UUIDMap
import Zwerg.Entity
import Zwerg.Event
import Zwerg.Generator
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Player.TestPlayer
import Zwerg.Log (Log, HasLog(..), emptyLog)
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Control.Lens (makeClassy, use, (.=), (%=), view)
import Control.Monad.Random (runRandT, RandT, MonadRandom)
import Unsafe (unsafeHead)

data GameState = GameState
  { _gsComponents :: Components
  , _gsUUIDGen :: UUIDGen
  , _gsLog :: Log
  , _gsPort :: Port
  , _gsGlyphMap :: GlyphMap
  , _gsEventQueue :: EventQueue
  }

makeClassy ''GameState

emptyGameState :: GameState
emptyGameState =
  GameState
  { _gsComponents = emptyComponents
  , _gsUUIDGen = initUUIDGen
  , _gsLog = emptyLog
  , _gsPort = initMainMenu
  , _gsGlyphMap = emptyGlyphMap
  , _gsEventQueue = zEmpty
  }

instance HasComponents GameState where
  components = gsComponents

instance HasUUIDGen GameState where
  uuidGen = gsUUIDGen

instance HasLog GameState where
  userLog = gsLog

instance HasPort GameState where
  port = gsPort

instance HasGlyphMap GameState where
  glyphMap = gsGlyphMap

instance HasEventQueue GameState where
  eventQueue = gsEventQueue

-- |test
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
  _ <- generate testSquareGenerator
  _ <- generate testSquareGenerator
  _ <- generate testSquareGenerator
  _ <- generate testSquareGenerator
  _ <- generate testSquareGenerator
  _ <- generate testSquareGenerator
  generate $ testPlayerGenerator lid

processUserInput :: KeyCode -> Game ()
processUserInput k = do
  currentPort <- use port
  processUserInput' currentPort k
  processEvents
  newPort <- use port
  when (newPort == MainScreen) $ do
    ts <- use (ticks . uuidMap)
    let (minTick, uuids) = getMinimumUUIDs ts
    traceShowM uuids
    (ticks . uuidMap) %= fmap (\x -> max (x - minTick) 0)
    updateGlyphMap

processUserInput' :: Port -> KeyCode -> Game ()
processUserInput' (MainMenu m) (KeyChar 'j') = port .= (MainMenu $ next m)
processUserInput' (MainMenu m) (KeyChar 'k') = port .= (MainMenu $ prev m)
processUserInput' (MainMenu m) Return =
  case (view label $ focus m) of
    "new game" -> generateGame >> port .= MainScreen
    "exit" -> port .= ExitScreen
    _ -> return ()
processUserInput' MainScreen (KeyChar 'h') =
  processPlayerDirectionInput Direction.Left
processUserInput' MainScreen (KeyChar 'j') =
  processPlayerDirectionInput Direction.Down
processUserInput' MainScreen (KeyChar 'k') =
  processPlayerDirectionInput Direction.Up
processUserInput' MainScreen (KeyChar 'l') =
  processPlayerDirectionInput Direction.Right
processUserInput' _ _ = return ()

updateGlyphMap :: Game ()
updateGlyphMap = do
  updatedGlyphs <- getGlyphMapUpdates
  glyphMap %= mergeGlyphMaps updatedGlyphs

processEvents :: Game ()
processEvents = do
  whenJustM popEvent $ \nextEvent -> do
    processEvent nextEvent
    processEvents

processEvent :: Event -> Game ()
processEvent (MoveEntityDirectionEvent ed) = do
  let uuid = moverUUID ed
      dir = direction ed
  (x, y) <- unPosition <$> demandComp position uuid
  let (x', y') =
        if | dir == Direction.Left -> (x - 1, y)
           | dir == Direction.Right -> (x + 1, y)
           | dir == Direction.Up -> (x, y - 1)
           | dir == Direction.Down -> (x, y + 1)
      destinationOutsideMap =
        x' < 0 || x' >= round mapWidth || y' < 0 || y' >= round mapHeight
  when (destinationOutsideMap && uuid /= playerUUID) $
    throwError $
    ZError __FILE__ __LINE__ Fatal "NPC Entity attempted to move outside of map"
  when (destinationOutsideMap && uuid == playerUUID) $
    throwError $
    ZError __FILE__ __LINE__ Warning "Player attempted to move outside of map"
  let oldPos = mkPosition (x, y)
      newPos = mkPosition (x', y')
  levelUUID <- demandComp level uuid
  levelTiles <- demandComp tileMap levelUUID
  newTileUUID <- TM.tileUUIDatPosition newPos levelTiles
  newTileBlocked <- demandComp blocked newTileUUID
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
  setComp uuid position newPos
  removeOccupant uuid oldTileUUID
  setComp oldTileUUID blocked False
  addOccupant uuid newTileUUID
  setComp newTileUUID blocked True
  setComp oldTileUUID needsRedraw True
  setComp newTileUUID needsRedraw True
processEvent (WeaponAttackAttemptEvent ed) = eraseEntity (view defenderUUID ed)
processEvent _ = return ()

getGlyphMapUpdates
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => m GlyphMap
getGlyphMapUpdates = do
  currentLevelUUID <- demandComp level playerUUID
  currentLevelTiles <- demandComp tiles currentLevelUUID
  tilesWithUpdatedNeeded <- zFilterM (demandComp needsRedraw) currentLevelTiles
  updatedGlyphs <-
    forM (zToList tilesWithUpdatedNeeded) $ \tileUUID -> do
      pos <- demandComp position tileUUID
      occUUID <- getPrimaryOccupant tileUUID
      gly <- demandComp glyph occUUID
      setComp tileUUID needsRedraw False
      return (pos, gly)
  return $ mkGlyphMap updatedGlyphs

processPlayerDirectionInput :: Direction.Direction -> Game ()
processPlayerDirectionInput dir =
  let processPlayerMove =
        pushEventM $
        MoveEntityDirectionEvent $ MoveEntityDirectionEventData playerUUID dir
      processPlayerAttack attackedUUID = do
        pushEventM $
          WeaponAttackAttemptEvent $
          WeaponAttackAttemptEventData playerUUID attackedUUID
  in getEntityPlayerAttacking dir >>=
     maybe processPlayerMove processPlayerAttack

getEntityPlayerAttacking
  :: (HasComponents s, MonadError ZError m, MonadState s m)
  => Direction.Direction -> m (Maybe UUID)
getEntityPlayerAttacking dir = do
  attackedTileUUID <- getPlayerTileUUID >>= getAdjacentTileUUID dir
  case attackedTileUUID of
    Just attackedTileUUID' -> do
      adjacentTileEnemyOccupants <-
        getTileOccupantsOfType attackedTileUUID' Enemy
      if | zIsNull adjacentTileEnemyOccupants -> return Nothing
         | zSize adjacentTileEnemyOccupants > 1 ->
           throwError $
           ZError __FILE__ __LINE__ Fatal "found multiple enemies on same tile"
         | otherwise ->
           return $ Just $ unsafeHead $ zToList adjacentTileEnemyOccupants
    Nothing -> return Nothing
