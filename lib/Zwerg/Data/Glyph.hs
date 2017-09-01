module Zwerg.Data.Glyph
    ( CellColor(..)
    , pattern PartialCellColor
    , pattern FullCellColor
    , Glyph(..)
    , pattern PartialGlyph
    , pattern FullGlyph
    , GlyphMapCell(..)
    , unpackGlyphMapCell
    , markVisibility
    ) where

import Zwerg.Prelude

data CellColor = CellColor ZColor (Maybe ZColor)
    deriving stock (Eq, Generic)
    deriving anyclass Binary

pattern FullCellColor :: ZColor -> ZColor -> CellColor
pattern FullCellColor fg bg = CellColor fg (Just bg)

pattern PartialCellColor :: ZColor -> CellColor
pattern PartialCellColor fg = CellColor fg Nothing

instance ZDefault CellColor where
    zDefault = CellColor darkred $ Just white

data Glyph = Glyph Char CellColor
    deriving stock (Eq, Generic)
    deriving anyclass Binary

pattern FullGlyph :: Char -> ZColor -> ZColor -> Glyph
pattern FullGlyph char fg bg = Glyph char (FullCellColor fg bg)

pattern PartialGlyph :: Char -> ZColor -> Glyph
pattern PartialGlyph char fg = Glyph char (PartialCellColor fg)

instance ZDefault Glyph where
    zDefault = Glyph zDefault zDefault

data GlyphMapCell = GlyphMapCell Bool Glyph Glyph
    deriving stock (Eq, Generic)
    deriving anyclass Binary

instance ZDefault GlyphMapCell where
    zDefault = GlyphMapCell True zDefault zDefault

{-# INLINABLE cellColors #-}
cellColors :: GlyphMapCell -> (ZColor, ZColor)
cellColors (GlyphMapCell True (FullGlyph _ fg bg) _) = (fg,bg)
cellColors (GlyphMapCell True (PartialGlyph _ fg) (FullGlyph _ _ bg)) = (fg,bg)
cellColors (GlyphMapCell False _ (FullGlyph _ fg bg)) = join bimap (darken 0.8) (fg,bg)
cellColors _ = (darkred, white)

{-# INLINABLE cellChar #-}
cellChar :: GlyphMapCell -> Char
cellChar (GlyphMapCell True (Glyph ch _) _) = ch
cellChar (GlyphMapCell False _ (Glyph ch _)) = ch

{-# INLINABLE unpackGlyphMapCell #-}
unpackGlyphMapCell :: GlyphMapCell -> (Char, ZColor, ZColor)
unpackGlyphMapCell gmc = let (fg,bg) = cellColors gmc in (cellChar gmc, fg, bg)

{-# INLINABLE markVisibility #-}
markVisibility :: Bool -> GlyphMapCell -> GlyphMapCell
markVisibility isVis (GlyphMapCell _ g1 g2) = GlyphMapCell isVis g1 g2
