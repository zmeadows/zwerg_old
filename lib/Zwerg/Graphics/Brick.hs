module Zwerg.Graphics.Brick where

import Zwerg.Prelude hiding ((<>))

import Zwerg
import Zwerg.Component
import Zwerg.Data.GridMap
import Zwerg.Data.HP
import Zwerg.Entity
import Zwerg.Event
import Zwerg.Game
import Zwerg.Log
import Zwerg.Random
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Brick.AttrMap
import Brick.Markup (markup)
import Brick.Util (on, fg)
import Brick.Widgets.Core
import Brick.Widgets.ProgressBar
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl1)
import Data.Monoid ((<>))
import Data.Text (append)
import Data.Text.Markup ((@@))
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.ProgressBar as BP
import qualified Data.Vector as Vec
import qualified Graphics.Vty as VTY

menuToBrickList :: Menu a -> BL.List () Text
menuToBrickList m =
  let allLabels = getMenuLabels m
  in BL.listMoveTo (getMenuFocusIndex m) $ BL.list () (Vec.fromList allLabels) 1

eventVTYtoZwergInput :: VTY.Event -> Maybe KeyCode
eventVTYtoZwergInput (VTY.EvKey VTY.KEsc []) = Just Escape
eventVTYtoZwergInput (VTY.EvKey (VTY.KChar ch) []) = Just $ KeyChar ch
eventVTYtoZwergInput (VTY.EvKey VTY.KEnter []) = Just Return
eventVTYtoZwergInput _ = Nothing

glyphToVtyImage :: (Glyph, Bool) -> VTY.Image
glyphToVtyImage (Glyph c fgC _ _ _, isVis) =
  if isVis
    then VTY.char (zwergColorToVtyColor fgC `on` VTY.Color240 220) c
    else VTY.char (zwergColorToVtyColor fgC `on` VTY.Color240 0) c

glyphMapToVtyImage :: GlyphMap -> VTY.Image
glyphMapToVtyImage gm =
  let rows = glyphMapToRows gm :: [[(Glyph, Bool)]]
      mkImageRow row = foldl1 (VTY.<|>) $ map glyphToVtyImage row
  in foldl1 (VTY.<->) $ map mkImageRow rows

makeStatsWidget :: Stats -> BT.Widget ()
makeStatsWidget s =
  vBox [go STR <+> go DEX, go INT <+> go CHA, go CON <+> go WIS]
    where go :: Stat -> BT.Widget ()
          go stat =
            let statTypeStr = append (show stat) ": "
                statValStr = leftPad 3 $ show $ lookupStat stat s
            in markup (statTypeStr @@ fg VTY.green) <+> (txt statValStr) <+> (txt " ")

makeHpWidget :: HP -> BT.Widget ()
makeHpWidget h =
  let (hpLeft, maxHP) = unwrap h
      hpLabel = show hpLeft ++ "/" ++ show maxHP
      hpRatio = (fromIntegral hpLeft :: Float) / (fromIntegral maxHP :: Float)
  in str "HP: " <+> hLimit 13 (BP.progressBar (Just hpLabel) hpRatio)

zwergColorToVtyColor :: Color -> VTY.Color
zwergColorToVtyColor zc =
  let mkVTYcolor :: (Int, Int, Int) -> VTY.Color
      mkVTYcolor (r, g, b) = VTY.rgbColor r g b
  in case zc of
       Green0 -> mkVTYcolor (159, 255, 128)
       Green1 -> mkVTYcolor (121, 255, 77)
       Green2 -> mkVTYcolor (83, 255, 26)
       Green3 -> mkVTYcolor (38, 153, 0)
       Blue0 -> mkVTYcolor (153, 153, 255)
       Blue1 -> mkVTYcolor (102, 102, 255)
       Blue2 -> mkVTYcolor (51, 51, 255)
       Blue3 -> mkVTYcolor (0, 0, 204)
       Red0 -> mkVTYcolor (255, 102, 102)
       Red1 -> mkVTYcolor (255, 51, 51)
       Red2 -> mkVTYcolor (230, 0, 0)
       Red3 -> mkVTYcolor (153, 0, 0)
       White0 -> VTY.Color240 227
       White1 -> VTY.Color240 230
       White2 -> VTY.Color240 233
       White3 -> VTY.Color240 236
       Black0 -> VTY.Color240 0
       Black1 -> VTY.Color240 217
       Black2 -> VTY.Color240 220
       Black3 -> VTY.Color240 223

type UIBuilder a = ExceptT ZError (Reader ZwergState) a

buildZwergUI :: ZwergState -> [BT.Widget ()]
buildZwergUI zs =
  let ps = view (gameState . portal) zs
   in case runReader (runExceptT (concat <$> mapM buildPortUI ps)) zs of
       Left _ -> []
       Right x -> x

--FIXME: refactor
buildPortUI :: Port -> UIBuilder [BT.Widget ()]

buildPortUI (DeathScreen deathMsg) = return [BC.vCenter $ BC.hCenter $ txt deathMsg]

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
  tileUUID <- fmap (atPos pos) $ level <~> playerUUID >>= (<~>) tileMap
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
        Just x -> BM.halt $ set errorMsg (Just $ show x) zs'
        Nothing -> BM.continue zs'
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
  s <- liftIO $ BM.defaultMain zwergApp $ set ranGen gen initZwergState
  liftIO $ print $ s ^. errorMsg

zwergLogo :: BT.Widget ()
zwergLogo = vBox $ map (withAttr customAttr)
                 [ txt " ______      _____ _ __ __ _ "
                 , txt "|_  /\\ \\ /\\ / / _ \\ '__/ _` |"
                 , txt " / /  \\ V  V /  __/ | | (_| |"
                 , txt "/___|  \\_/\\_/ \\___|_|  \\__, |"
                 , txt "                        __/ |"
                 , txt "                       |___/ "
                 ]
