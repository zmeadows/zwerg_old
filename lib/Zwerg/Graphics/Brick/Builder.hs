module Zwerg.Graphics.Brick.Builder where

import Zwerg.Prelude hiding ((<>))

import Zwerg
import Zwerg.Component
import Zwerg.Entity
import Zwerg.Game
import Zwerg.Log
import Zwerg.Data.GridMap
import Zwerg.UI.GlyphMap
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Data.Either (either)
import Data.Foldable (foldl1)
import Data.Monoid ((<>))
import Data.Text (append)
import Data.Text.Markup ((@@))

--TODO: add import lists to these
-- import Brick.AttrMap
import Brick.Markup (markup)
import Brick.Util (on, fg)
import Brick.Widgets.Core
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.ProgressBar as BP
import qualified Data.Vector as V
import qualified Graphics.Vty as VTY

listDrawElement :: Bool -> Text -> BT.Widget ()
listDrawElement sel a =
  let selStr s = if sel then withAttr "logo" (str s) else str s
  in BC.hCenter $ selStr (unpack a)

menuToBrickList :: Menu a -> BL.List () Text
menuToBrickList m =
  let allLabels = getMenuLabels m
  in BL.listMoveTo (getMenuFocusIndex m) $ BL.list () (V.fromList allLabels) 1

newtype UIBuilder' a = UIBuilder (ExceptT ZError (Reader GameState) a)
  deriving (Functor , Applicative , Monad , MonadReader GameState, MonadError ZError)

type UIBuilder a = HasCallStack => UIBuilder' a

runUIBuilder :: UIBuilder a -> GameState -> Either ZError a
runUIBuilder (UIBuilder x) gs = runReader (runExceptT x) gs

buildZwergUI :: ZwergState -> [BT.Widget ()]
buildZwergUI zs =
  either (const []) id $ runUIBuilder (concat <$> (mapM buildPortUI (view portal zs))) $ view gameState zs

--FIXME: refactor
buildPortUI :: Port -> UIBuilder [BT.Widget ()]

buildPortUI (DeathScreen deathMsg) = return $! [BC.vCenter $ BC.hCenter $ txt deathMsg]

buildPortUI (MainMenu m) = return $! [ui]
  where
    l = menuToBrickList m
    box = BB.border $ hLimit 24 $ vLimit 5 $ BL.renderList listDrawElement True l
    ui = BC.vCenter $ vBox [BC.hCenter $ zwergLogo, str " ", BC.hCenter box]

buildPortUI (ViewInventory m) = do
    menuWidget <- buildMenuWidget
    return $ [BC.vCenterLayer $ BC.hCenterLayer $ hLimit 40 $ BB.border $ menuWidget]

  where focusedUUID = view (item . _1) $ focus m :: UUID

        makeItemWidget :: (UUID, Bool) -> UIBuilder (BT.Widget ())
        makeItemWidget (itemUUID, markedForDrop) = do
          itemName <- name <~> itemUUID
          let isFocused = itemUUID == focusedUUID
              attr = if | isFocused && (not markedForDrop)       -> "inventory" <> "focused"
                        | (not isFocused) && (not markedForDrop) -> "inventory" <> "unfocused"
                        | isFocused && markedForDrop             -> "inventory" <> "drop_focused"
                        | (not isFocused) && markedForDrop       -> "inventory" <> "drop_unfocused"
              nameWidget = padRight BT.Max $ withAttr attr $ txt itemName
              checkWidget = withAttr attr $ if markedForDrop then txt "*" else txt "-"
          return $ nameWidget <+> checkWidget

        buildMenuWidget = foldr1 (<=>) <$> traverse makeItemWidget m

-- TODO: take background color from tile?
-- TODO: check that name/description exist and just don't show them if they don't
buildPortUI (ExamineTiles pos) = do
  tileUUID <- fmap (atPos pos) $ level <~> playerUUID >>= (<~>) tileMap
  occUUID <- getPrimaryOccupant tileUUID
  _name <- name <~> occUUID
  _desc <- description <~> occUUID
  fear <- getFearLevel occUUID
  --TODO: only show description if player can see tile
  let (x,y) = unwrap pos
      tileMaker = translateBy (BT.Location (x,y)) $ withAttr "keyword1" $ txt "X"
      sometext  = translateBy (BT.Location (0,mapHeightINT+1))
                    $ vBox [txt _name, txt _desc, txt fear]
      blankLog  = translateBy (BT.Location (0,mapHeightINT+1)) (padBottom BT.Max $ padRight BT.Max $ fill ' ')
  return $! [tileMaker, sometext, blankLog]

buildPortUI (MainScreen gm) = do
  _name  <- name <~> playerUUID
  _stats <- stats <~> playerUUID
  _hp    <- hp <~> playerUUID
  mylog <- view (gameState . userLog)
  let mapWidget :: BT.Widget ()
      mapWidget =
        let rows = glyphMapToRows gm :: [[(Glyph, Bool)]]
            glyphToVtyImage :: (Glyph, Bool) -> VTY.Image
            glyphToVtyImage (Glyph c (CellColor fgC _) _, isVis) =
              if isVis
                then VTY.char (zwergColorToVtyColor fgC `on` VTY.Color240 220) c
                else VTY.char (zwergColorToVtyColor fgC `on` VTY.Color240 0) c
            mkImageRow :: [(Glyph, Bool)] -> VTY.Image
            mkImageRow row = foldl1 (VTY.<|>) $ map glyphToVtyImage row
         in raw $ foldl1 (VTY.<->) $ map mkImageRow rows

      logWidget = vBox $ (str . unpack) <$> concat (splitLog 50 15 mylog) :: BT.Widget ()

      statsWidget = vBox [go STR <+> go DEX, go INT <+> go CHA, go CON <+> go WIS] :: BT.Widget ()

      go :: Stat -> BT.Widget ()
      go stat = let statTypeStr = append (show stat) ": "
                    statValStr = leftPad 3 $ show $ lookupStat stat _stats
                in markup (statTypeStr @@ fg VTY.green) <+> (txt statValStr) <+> (txt " ")

      hpWidget :: BT.Widget ()
      hpWidget = let (hpLeft, maxHP) = unwrap _hp
                     hpLabel = show hpLeft ++ "/" ++ show maxHP
                     hpRatio = (fromIntegral hpLeft :: Float) / (fromIntegral maxHP :: Float)
                 in str "HP: " <+> hLimit 13 (BP.progressBar (Just hpLabel) hpRatio)

      nameWidget = markup $ _name @@ fg VTY.yellow :: BT.Widget ()

  return $! [
    vLimit mapHeightINT (
      mapWidget <+> BB.vBorder <+> (
        nameWidget <=> hpWidget <=> statsWidget
        )
    ) <=> (BB.hBorder <=> logWidget)
    ]

buildPortUI _ = return $! [emptyWidget]

zwergLogo :: BT.Widget ()
zwergLogo = vBox $ map (withAttr "logo")
                 [ txt " ______      _____ _ __ __ _ "
                 , txt "|_  /\\ \\ /\\ / / _ \\ '__/ _` |"
                 , txt " / /  \\ V  V /  __/ | | (_| |"
                 , txt "/___|  \\_/\\_/ \\___|_|  \\__, |"
                 , txt "                        __/ |"
                 , txt "                       |___/ "
                 ]


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
