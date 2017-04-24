module Zwerg.Graphics.Brick where

import Zwerg.Component.HP
import Zwerg.Prelude hiding (on)
import Zwerg.UI.GlyphMap
import Zwerg.UI.Input
import Zwerg.UI.Menu

import Brick.Widgets.Core

import Brick.Markup (markup)
import qualified Brick.Types as BT
import Brick.Util (on, fg)
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.ProgressBar as BP
import Data.Foldable (foldl1)
import qualified Data.Vector as Vec

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
  in str "HP: " <+> hLimit 5 (BP.progressBar (Just hpLabel) hpRatio)

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
