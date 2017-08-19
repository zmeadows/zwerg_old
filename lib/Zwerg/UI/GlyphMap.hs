module Zwerg.UI.GlyphMap where

import Zwerg.Prelude

import Zwerg.Data.Glyph
import Zwerg.Data.GridMap
import Zwerg.Util

data GlyphMapCell = GlyphMapCell
    { _glyphMapCellIsVisible        :: Bool
    , _glyphMapCellVisibleGlyph     :: Glyph
    , _glyphMapCellChar             :: Char
    , _glyphMapCellFoggedForeground :: ZColor
    , _glyphMapCellFoggedBackground :: ZColor
    } deriving (Eq, Generic)
makeFields ''GlyphMapCell
instance Binary GlyphMapCell

getCellColors :: GlyphMapCell -> (ZColor, ZColor)
getCellColors (GlyphMapCell isVis vg _ fogFG fogBG) =
    if isVis
       then case vg ^. (cellColor . bgColor) of
              Just visBG -> (vg ^. (cellColor . fgColor), visBG)
              Nothing -> (vg ^. (cellColor . fgColor), fogBG)
       else (darken 0.5 fogFG, darken 0.5 fogBG)

getGlyphChar :: GlyphMapCell -> Char
getGlyphChar (GlyphMapCell isVis vg fogCH _ _) =
    if isVis
       then vg ^. char
       else fogCH

type GlyphMap = GridMap GlyphMapCell

class HasGlyphMap s where
  glyphMap :: Lens' s GlyphMap

blankGlyphMap :: GlyphMap
blankGlyphMap = zBuild (const emptyCellData)
    where emptyGlyph = Glyph ' ' (CellColor black $ Just black)
          emptyCellData = GlyphMapCell False emptyGlyph ' ' black black

glyphMapToRows :: GlyphMap -> [[GlyphMapCell]]
glyphMapToRows = chunksOf mapWidthINT . zElems

