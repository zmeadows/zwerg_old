module Zwerg.Graphics.Brick.Builder where

import Zwerg.Prelude hiding ((<>))

import Zwerg
import Zwerg.Component
import Zwerg.Data.Glyph
import Zwerg.Entity
import Zwerg.Entity.Compare
import Zwerg.Game
import Zwerg.Log
import Zwerg.UI.GlyphMap
import Zwerg.UI.Menu
import Zwerg.UI.Port
import Zwerg.Util

import Data.Foldable (foldl1)
import Data.Monoid ((<>))
import Data.Text.Markup ((@@))

--TODO: add import lists to these
-- import Brick.AttrMap
import Brick.Markup (markup)
import Brick.Util (on, fg)
import Brick.Widgets.Core hiding (visible)
import qualified Brick.Types as BT
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Center as BC
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.ProgressBar as BP
import qualified Data.Vector as V
import qualified Graphics.Vty as VTY

import Lens.Micro.Platform (view)

listDrawElement :: Bool -> Text -> BT.Widget ()
listDrawElement sel a =
  let selStr s = if sel then withAttr "logo" (str s) else str s
  in BC.hCenter $ selStr (unpack a)

menuToBrickList :: Menu a -> BL.List () Text
menuToBrickList m =
  let allLabels = getMenuLabels m
  in BL.listMoveTo (getMenuFocusIndex m) $ BL.list () (V.fromList allLabels) 1

newtype UIBuilder' a = UIBuilder (Reader GameState a)
    deriving newtype (Functor, Applicative, Monad, MonadReader GameState)

type UIBuilder a = HasCallStack => UIBuilder' a

runUIBuilder :: UIBuilder a -> GameState -> a
runUIBuilder (UIBuilder x) gs = runReader x gs

buildZwergUI :: ZwergState -> [BT.Widget ()]
buildZwergUI zs = runUIBuilder (concat <$> (mapM buildPortUI (view portal zs))) $ view gameState zs

--FIXME: refactor
buildPortUI :: Port -> UIBuilder [BT.Widget ()]

buildPortUI (DeathScreen deathMsg) = return $ [BC.vCenter $ BC.hCenter $ txt deathMsg]

buildPortUI (MainMenu m) = return $ [ui]
  where
    l = menuToBrickList m
    box = BB.border $ hLimit 24 $ vLimit 5 $ BL.renderList listDrawElement True l
    ui = BC.vCenter $ vBox [BC.hCenter $ zwergLogo, str " ", BC.hCenter box]

buildPortUI (ViewInventory m) = do
    menuWidget <- buildMenuWidget
    return $ [BC.vCenterLayer $ BC.hCenterLayer $ hLimit 40 $ BB.border $ menuWidget]

  where focusedUUID = fst $ item $ focus m :: UUID

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
  tileUUID <- (`zAt` pos) <$> (level <~> playerUUID >>= (<~>) tileMap)
  occUUID <- getPrimaryOccupant tileUUID
  (_name, _desc) <- (name, description) <~!!> occUUID
  fear <- getFearLevel occUUID
  --TODO: only show description if player can see tile
  let (x,y) = unwrap pos
      tileMaker = translateBy (BT.Location (x,y)) $ withAttr "keyword1" $ txt "X"
      sometext  = translateBy (BT.Location (0,mapHeightINT+1))
                    $ vBox [txt _name, txt _desc, txt fear]
      blankLog  = translateBy (BT.Location (0,mapHeightINT+1)) (padBottom BT.Max $ padRight BT.Max $ fill ' ')
  return $ [tileMaker, sometext, blankLog]

buildPortUI (MainScreen gm) = do
  _name  <- name <~> playerUUID
  _stats <- stats <~> playerUUID
  _hp    <- hp <~> playerUUID
  mylog <- view (gameState . userLog)
  let mapWidget :: BT.Widget ()
      mapWidget =
        let rows = glyphMapToRows gm :: [[GlyphMapCell]]
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

  return $ [
    vLimit mapHeightINT (
      mapWidget <+> BB.vBorder <+> (
        nameWidget <=> hpWidget <=> statsWidget
        )
    ) <=> (BB.hBorder <=> logWidget)
    ]

buildPortUI _ = return [emptyWidget]

zwergLogo :: BT.Widget ()
zwergLogo = vBox $ map (withAttr "logo")
                 [ txt " ______      _____ _ __ __ _ "
                 , txt "|_  /\\ \\ /\\ / / _ \\ '__/ _` |"
                 , txt " / /  \\ V  V /  __/ | | (_| |"
                 , txt "/___|  \\_/\\_/ \\___|_|  \\__, |"
                 , txt "                        __/ |"
                 , txt "                       |___/ "
                 ]


zwergColorToVtyColor :: ZColor -> VTY.Color
zwergColorToVtyColor (ZColor r g b) = VTY.rgbColor r g b

mkImageRow :: [GlyphMapCell] -> VTY.Image
mkImageRow [] = VTY.emptyImage
mkImageRow (cd:cds) = mkImageRow' cds (initFG, initBG) (singleton initChar) []
    where (initChar, initFG, initBG) = unpackGlyphMapCell cd

mkImageRow' :: [GlyphMapCell] -> (ZColor, ZColor) -> Text -> [VTY.Image] -> VTY.Image
mkImageRow' [] curCellColor chars imgs = foldl1 (VTY.<|>) $ reverse $ (cellsToImage chars curCellColor) : imgs

mkImageRow' (r:rs) (curFG, curBG) chars imgs =
    if (nextFG, nextBG) == (curFG, curBG)
       then mkImageRow' rs (curFG, curBG) (chars <> singleton nextChar) imgs
       else mkImageRow' rs (nextFG, nextBG) (singleton nextChar) $ (cellsToImage chars (curFG,curBG)) : imgs
  where (nextChar, nextFG, nextBG) = unpackGlyphMapCell r

cellsToImage :: Text -> (ZColor,ZColor) -> VTY.Image
cellsToImage t (fgC, bgC) = VTY.text' (zwergColorToVtyColor fgC `on` zwergColorToVtyColor bgC) t
