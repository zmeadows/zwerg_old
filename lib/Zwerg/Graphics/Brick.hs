module Zwerg.Graphics.Brick where

import Zwerg.Prelude hiding (on)

import Zwerg.Data.HP
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu
import Zwerg.Util

import Brick.Markup (markup)
import Brick.Util (on, fg)
import Brick.Widgets.Core
import Data.Foldable (foldl1)
import Data.Text (append)
import Data.Text.Markup ((@@))
import qualified Brick.Types as BT
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
