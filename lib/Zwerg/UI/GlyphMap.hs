module Zwerg.UI.GlyphMap where

-- import Zwerg.Data.Position
import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Util

data CellData = CellData
    { _isVisible :: Bool
    , _fogged    :: Glyph
    , _visible   :: Glyph
    } deriving (Generic)

instance Binary CellData
makeClassy ''CellData

type GlyphMap = GridMap CellData

class HasGlyphMap s where
  glyphMap :: Lens' s GlyphMap

blankGlyphMap :: GlyphMap
blankGlyphMap = zBuild (const emptyCellData)
  where emptyGlyph = Glyph ' ' (CellColor Black0 Black0) $ Just (CellColor Black0 Black0)
        emptyCellData = CellData False emptyGlyph emptyGlyph

glyphMapToRows :: GlyphMap -> [[CellData]]
glyphMapToRows = chunksOf mapWidthINT . zElems

