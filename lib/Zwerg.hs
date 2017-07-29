module Zwerg (zwerg) where

import Zwerg.Component
import Zwerg.Component.TileMap
import Zwerg.Event
import Zwerg.Entity
import Zwerg.Game
import Zwerg.Graphics.Brick
import Zwerg.Log
import Zwerg.Prelude hiding (on, ByteString)
import Zwerg.Random
import Zwerg.UI.Input
import Zwerg.UI.Port

import Data.ByteString.Lazy (ByteString)
-- import Data.Binary (encode)
-- import Codec.Compression.Zlib (compress)

import Debug (error)

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
import Brick.Widgets.ProgressBar

data ZwergState = ZwergState
  { _zsGameState :: GameState
  , _ranGen :: RanGen
  , _quitting :: Bool
  , _pastState :: [ByteString]
  }
makeClassy ''ZwergState

instance HasGameState ZwergState where
  gameState = zsGameState
instance HasComponents ZwergState where
  components = gameState . components
instance HasLog ZwergState where
  userLog = gameState . userLog

initZwergState :: ZwergState
initZwergState = ZwergState
  { _zsGameState = emptyGameState
  , _ranGen = pureRanGen 0
  , _quitting = False
  , _pastState = []
  }

zwerg :: IO ()
zwerg = initBrick

type UIBuilder a = ExceptT ZError (Reader ZwergState) a

buildZwergUI :: ZwergState -> [BT.Widget ()]
buildZwergUI zs =
  let ps = view (gameState . portal) zs
   in case runReader (runExceptT (concat <$> mapM buildPortUI ps)) zs of
       Left _ -> []
       Right x -> x

--FIXME: refactor
buildPortUI :: Port -> UIBuilder [BT.Widget ()]

buildPortUI (MainMenu m) = return [ui]
  where
    l = menuToBrickList m
    box = BB.border $ hLimit 24 $ vLimit 5 $ BL.renderList listDrawElement True l
    ui = BC.vCenter $ vBox [BC.hCenter $ zwergLogo, str " ", BC.hCenter box]

buildPortUI (ViewInventory m) = return [ui]
  where
    l = menuToBrickList m
    box = BB.border $ hLimit 24 $ vLimit 5 $ BL.renderList listDrawElement True l
    ui = BC.vCenterLayer $ BC.hCenterLayer $ box

-- TODO: take background color from tile?
-- TODO: check that name/description exist and just don't show them if they don't
buildPortUI (ExamineTiles pos) = do
  tileUUID <- level <~> playerUUID >>= (<~>) tileMap >>= tileUUIDatPosition pos
  occUUID <- getPrimaryOccupant tileUUID
  thisName <- name <~> occUUID
  thisDesc <- description <~> occUUID
  thisFearLevel <- getFearLevel occUUID
  --TODO: only show description if player can see tile
  let (x,y) = unwrap pos
      tileMaker = translateBy (BT.Location (x,y)) $ withAttr "keyword1" $ txt "X"
      sometext  = translateBy (BT.Location (0,mapHeightINT+1))
                    $ vBox [txt thisName, txt thisDesc, txt thisFearLevel]
      blankLog  = translateBy (BT.Location (0,mapHeightINT+1)) (padBottom BT.Max $ padRight BT.Max $ fill ' ')
  return [tileMaker, sometext, blankLog]

buildPortUI (MainScreen gm) = do
  let uiMap = raw $ glyphMapToVtyImage gm
  pName <- name <~> playerUUID
  pStats <- stats <~> playerUUID
  pHP <- hp <~> playerUUID
  uLog <- view userLog
  return $ [vLimit mapHeightINT
      (uiMap <+> BB.vBorder <+>
       (markup (pName @@ fg VTY.yellow) <=> makeHpWidget pHP <=>
        makeStatsWidget pStats)) <=>
    (BB.hBorder <=> makeLogWidget uLog)]

buildPortUI _ = return [emptyWidget]

makeLogWidget :: Log -> BT.Widget ()
makeLogWidget l = vBox $ (str . unpack) <$> concat (splitLog 50 15 l)

listDrawElement :: Bool -> Text -> BT.Widget ()
listDrawElement sel a =
  let selStr s = if sel then withAttr customAttr (str s) else str s
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
          -- hist = view pastState zs
          (st', err, gen') = runGame (processUserInput kc) gen st
          -- zs' = set pastState ((compress (encode st)):hist) $ set gameState st' $ set ranGen gen' zs
          zs' = set gameState st' $ set ranGen gen' zs
      case err of
        --TODO: if player warning, print to log
        -- if engine failure, print to file and exit
        Just x -> error (show x)
        Nothing -> BM.continue zs'
          -- liftIO $ encodeFile "binary_components.dat" (zs' ^. components)
    _ -> BM.continue zs
handleEventZwerg a b = BM.resizeOrQuit a b

theMap :: AttrMap
theMap = attrMap VTY.defAttr
    [ ("keyword1", fg VTY.magenta)
    , ("keyword2", VTY.white `Brick.Util.on` VTY.blue)
    , (customAttr, fg VTY.red)
    , (progressCompleteAttr, VTY.black `on` VTY.green)
    , (progressIncompleteAttr, VTY.black `on` VTY.red)
    ]

zwergApp :: BM.App ZwergState ZwergEvent ()
zwergApp = BM.App
  { BM.appDraw         = buildZwergUI
  , BM.appHandleEvent  = handleEventZwerg
  , BM.appStartEvent   = return
  , BM.appAttrMap      = const theMap
  , BM.appChooseCursor = BM.neverShowCursor
  }

initBrick :: MonadIO m => m ()
initBrick = do
  gen <- newPureRanGen
  void $ liftIO $ BM.defaultMain zwergApp $ initZwergState {_ranGen = gen}

zwergLogo :: BT.Widget ()
zwergLogo = vBox $ map (withAttr customAttr)
                 [ txt " ______      _____ _ __ __ _ "
                 , txt "|_  /\\ \\ /\\ / / _ \\ '__/ _` |"
                 , txt " / /  \\ V  V /  __/ | | (_| |"
                 , txt "/___|  \\_/\\_/ \\___|_|  \\__, |"
                 , txt "                        __/ |"
                 , txt "                       |___/ "
                 ]
