module Zwerg.Data.Glyph where

import Zwerg.Prelude

data CellColor = CellColor
    { _fgColor :: ZColor
    , _bgColor :: Maybe ZColor
    } deriving (Eq, Generic)
makeClassy ''CellColor
instance Binary CellColor

data Glyph = Glyph
    { _glyphChar   :: Char
    , _glyphCellColorG :: CellColor
    } deriving (Eq, Generic)
makeFields ''Glyph
instance Binary Glyph

instance HasCellColor Glyph where
    cellColor = cellColorG

instance ZDefault Glyph where
    zDefault = Glyph 'X' (CellColor darkred $ Just white)

-- darken :: HasCellColor s a => a -> a
-- darken = undefined

-- getGlyph :: CellData -> Glyph
-- getGlyph (CellData isVis fg vg) = if isVis then vg else fg
--
-- getGlyphChar :: CellData -> Char
-- getGlyphChar cd = glyphChar . getGlyph
--
