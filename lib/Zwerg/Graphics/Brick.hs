module Zwerg.Graphics.Brick where

import Zwerg.Class
import Zwerg.Component.Glyph
import Zwerg.Component.HP
import Zwerg.Component.Stats
import Zwerg.Data.Color
import Zwerg.Prelude hiding (on)
import Zwerg.UI.Font
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu

import Brick.Widgets.Core

-- import Brick.AttrMap (attrMap, AttrMap)
-- import qualified Brick.Main as BM
import Brick.Markup (markup, (@?))
import qualified Brick.Types as BT
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.ProgressBar as BP
import Data.Foldable (foldl1)
import qualified Data.Vector as Vec

-- import Brick.Widgets.Core ((<=>), (<+>), padLeft)
import Control.Lens (Lens', makeClassy, view)

import Brick.Util (on, fg, bg)

import Data.Text.Markup ((@@))
import qualified Graphics.Vty as VTY

data BrickMainMenuContext =
  BrickMainMenuContext (Menu ())
  deriving (Show, Eq)

data BrickContext =
  BrickContext Int
  deriving (Show, Eq)

makeClassy ''BrickContext

brick
  :: HasBrickContext s
  => Lens' s BrickContext
brick = brickContext

uninitializedBrickContext :: BrickContext
uninitializedBrickContext = BrickContext 0

menuToBrickList :: Menu a -> BL.List () Text
menuToBrickList m =
  let allLabels = getMenuLabels m
  in BL.listMoveTo (getMenuFocusIndex m) $ BL.list () (Vec.fromList allLabels) 1

eventVTYtoZwergInput :: VTY.Event -> Maybe KeyCode
eventVTYtoZwergInput (VTY.EvKey VTY.KEsc []) = Just Escape
eventVTYtoZwergInput (VTY.EvKey (VTY.KChar ch) []) = Just $ KeyChar ch
eventVTYtoZwergInput (VTY.EvKey VTY.KEnter []) = Just Return
eventVTYtoZwergInput _ = Nothing

glyphToVtyImage :: Glyph -> VTY.Image
glyphToVtyImage (Glyph ft c col) =
  VTY.char (vtyCol `on` VTY.rgbColor 10 10 10) c
  where
    fontTypeToVTYstyle Bold = VTY.bold
    fontTypeToVTYstyle Standout = VTY.standout
    fontTypeToVTYstyle Underline = VTY.underline
    fontTypeToVTYstyle Dim = VTY.dim
    fontTypeToVTYstyle Blink = VTY.blink
    fontTypeToVTYstyle _ = VTY.dim -- VTY.defaultStyleMask
    vtyCol =
      let (r, g, b) = toRGB col
      in VTY.rgbColor r g b

glyphMapToVtyImage :: GlyphMap -> VTY.Image
glyphMapToVtyImage gm =
  let rows = glyphMapToRows gm :: [[Glyph]]
      mkImageRow row = foldl1 (VTY.<|>) $ map glyphToVtyImage row
  in foldl1 (VTY.<->) $ map mkImageRow rows

makeStatsWidget :: Stats -> BT.Widget ()
makeStatsWidget s =
  ((markup ("STR: " @@ fg VTY.green) <+> str (show $ lookupStat STR s)) <+>
   (markup ("DEX: " @@ fg VTY.green) <+> str (show $ lookupStat DEX s))) <=>
  ((markup ("INT: " @@ fg VTY.green) <+> str (show $ lookupStat INT s)) <+>
   (markup ("CHA: " @@ fg VTY.green) <+> str (show $ lookupStat CHA s))) <=>
  ((markup ("CON: " @@ fg VTY.green) <+> str (show $ lookupStat CON s)) <+>
   (markup ("WIS: " @@ fg VTY.green) <+> str (show $ lookupStat WIS s)))

makeHpWidget :: HP -> BT.Widget ()
makeHpWidget h =
  let (hpLeft, maxHP) = unwrap h
      hpLabel = show hpLeft ++ "/" ++ show maxHP
      hpRatio = (fromIntegral hpLeft :: Float) / (fromIntegral maxHP :: Float)
  in (str "HP: ") <+> hLimit 5 (BP.progressBar (Just hpLabel) hpRatio)
