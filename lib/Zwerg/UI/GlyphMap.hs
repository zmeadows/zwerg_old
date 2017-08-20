module Zwerg.UI.GlyphMap where

import Zwerg.Prelude

import Zwerg.Data.Glyph
import Zwerg.Data.GridMap
import Zwerg.Util

type GlyphMap = GridMap GlyphMapCell

blankGlyphMap :: GlyphMap
blankGlyphMap = zBuild (const emptyCellData)
    where emptyGlyph = Glyph ' ' (CellColor black $ Just black)
          emptyCellData = GlyphMapCell False emptyGlyph emptyGlyph

glyphMapToRows :: GlyphMap -> [[GlyphMapCell]]
glyphMapToRows = chunksOf mapWidthINT . zElems

