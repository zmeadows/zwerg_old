module Zwerg.Component.UUID (
    UUID(..),
    mkUUID,
    unUUID
    ) where

import Control.Exception.Base (assert)
import GHC.Generics (Generic)
import Data.Binary

newtype UUID = MkUUID Int
    deriving (Show, Read, Eq, Num, Ord, Generic)

instance Binary UUID

{-# INLINABLE mkUUID #-}
mkUUID :: Int -> UUID
mkUUID i = assert (i >= 0) (MkUUID i)

{-# INLINABLE unUUID #-}
unUUID :: UUID -> Int
unUUID (MkUUID i) = i
