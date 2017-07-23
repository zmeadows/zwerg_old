module Zwerg (zwerg) where

import Zwerg.Component
import Zwerg.Event
import Zwerg.Game
import Zwerg.Graphics.Brick
import Zwerg.Log
import Zwerg.Prelude
import Zwerg.Random
import Zwerg.UI.Input
import Zwerg.UI.Port

import Brick.AttrMap
import qualified Brick.Main as BM
import Brick.Markup (markup)
import qualified Brick.Types as BT
import Brick.Util (on, fg)
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Center as BC
import Brick.Widgets.Core
import qualified Brick.Widgets.List as BL
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Text.Markup ((@@))
import qualified Graphics.Vty as VTY

data ZwergState = ZwergState
  { _zsBrickContext :: BrickContext
  , _zsGameState :: GameState
  , _ranGen :: RanGen
  , _quitting :: Bool
  }

makeClassy ''ZwergState

instance HasGameState ZwergState where
  gameState = zsGameState

instance HasBrickContext ZwergState where
  brickContext = zsBrickContext

instance HasComponents ZwergState where
  components = gameState . components

instance HasLog ZwergState where
  userLog = gameState . userLog

initZwergState :: ZwergState
initZwergState =
  ZwergState
  { _zsBrickContext = uninitializedBrickContext
  , _zsGameState = emptyGameState
  , _ranGen = pureRanGen 0
  , _quitting = False
  }

zwerg :: IO ()
zwerg = initBrick

type UIBuilder a = ExceptT ZError (Reader ZwergState) a

buildZwergUI :: ZwergState -> [BT.Widget ()]
buildZwergUI zs =
  let ps = view (gameState . portal) zs
  in case runReader (runExceptT (mapM buildPortUI ps)) zs of
       Left _ -> []
       Right x -> x

buildPortUI :: Port -> UIBuilder (BT.Widget ())
buildPortUI (MainMenu m) = return ui
  where
    l = menuToBrickList m
    box =
      BB.border $ hLimit 25 $ vLimit 5 $ BL.renderList listDrawElement True l
    ui = BC.vCenter $ vBox [BC.hCenter $ str "zwerg", str " ", BC.hCenter box]
buildPortUI (MainScreen gm) = do
  let uiMap = raw $ glyphMapToVtyImage gm
  pName <- name <~> playerUUID
  pStats <- stats <~> playerUUID
  pHP <- hp <~> playerUUID
  uLog <- view userLog
  return $
    vLimit
      mapHeightINT
      (uiMap <+>
       BB.vBorder <+>
       (markup (pName @@ fg VTY.yellow) <=> makeHpWidget pHP <=>
        makeStatsWidget pStats)) <=>
    (BB.hBorder <=> makeLogWidget uLog)
buildPortUI _ = return emptyWidget

makeLogWidget :: Log -> BT.Widget ()
makeLogWidget l = vBox $ (str . unpack) <$> concat (splitLog 50 15 l)

listDrawElement :: Bool -> Text -> BT.Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str s)
          else str s
  in BC.hCenter $ selStr (unpack a)

customAttr :: AttrName
customAttr = BL.listSelectedAttr <> "custom"

handleEventZwerg :: ZwergState
                 -> BT.BrickEvent () ZwergEvent
                 -> BT.EventM () (BT.Next ZwergState)
handleEventZwerg zs (BT.VtyEvent ev) =
  case eventVTYtoZwergInput ev of
    Just Escape -> BM.halt zs
    Just kc -> do
      let st = view gameState zs
          gen = view ranGen zs
          (st', err, gen') = runGame (processUserInput kc) gen st
          zs' = set gameState st' $ set ranGen gen' zs
      case err of
        Just _ -> BM.halt zs'
        Nothing -> BM.continue zs'
          -- liftIO $ encodeFile "binary_components.dat" (zs' ^. components)
    _ -> BM.continue zs
handleEventZwerg a b = BM.resizeOrQuit a b

theMap :: AttrMap
theMap =
  attrMap
    VTY.defAttr
    [ ("keyword1", fg VTY.magenta)
    , ("keyword2", VTY.white `Brick.Util.on` VTY.blue)
    , (customAttr, fg VTY.red)
    ]

zwergApp :: BM.App ZwergState ZwergEvent ()
zwergApp =
  BM.App
  { BM.appDraw = buildZwergUI
  , BM.appHandleEvent = handleEventZwerg
  , BM.appStartEvent = return
  , BM.appAttrMap = const theMap
  , BM.appChooseCursor = BM.neverShowCursor
  }

initBrick
  :: MonadIO m
  => m ()
initBrick = do
  gen <- newPureRanGen
  void $ liftIO $ BM.defaultMain zwergApp $ initZwergState {_ranGen = gen}
-- mainLoop :: Zwerg ()
-- mainLoop = return ()
  -- whenJustM (fmap SDL.eventPayload <$> SDL.pollEvent) $ \case
  --   SDL.KeyboardEvent ked ->
  --     whenJust (keyboardEventToKey ked) $ \keycode -> do
  --       st <- use gameState
  --       gen <- use ranGen
  --       let (st', err, gen') = runGame (processUserInput keycode) gen st
  --       assign gameState st'
  --       assign ranGen gen'
  --       currentPort <- use (gameState . port)
  --       drawZwergScreen currentPort
  --       whenJust err $ \err' -> do
  --         liftIO $ print err'
  --         assign quitting True
  --   SDL.QuitEvent -> assign quitting True
  --   _ -> return ()
  -- use quitting >>= \q ->
  --   if q
  --     then quitZwerg
  --     else mainLoop
-- drawZwergScreen :: Port -> Zwerg ()
-- drawZwergScreen (MainMenu m) = do
--   ren <- use (sdl . renderer)
--   SDL.clear ren
--   drawMainMenu m
--   SDL.present ren
-- drawZwergScreen MainScreen = do
--   ren <- use (sdl . renderer)
--   gm <- use (gameState . glyphMap)
--   drawMainScreen gm
--   SDL.present ren
-- drawZwergScreen _ = return ()
-- quitZwerg :: Zwerg ()
-- quitZwerg = do
--   shutdownSDL
