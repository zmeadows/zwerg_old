module Zwerg.Data.Color
  ( Color
  , mkColor
  ) where

import Zwerg.Prelude

import Data.Binary
import Data.Word (Word8)
import GHC.Generics (Generic)

newtype Color =
  MkColor Word8
  deriving (Show, Read, Eq, Generic)

instance Binary Color

{-# INLINABLE mkColor #-}
mkColor :: Word8 -> Color
mkColor = MkColor
