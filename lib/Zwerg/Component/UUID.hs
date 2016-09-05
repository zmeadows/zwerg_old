module Zwerg.Component.UUID (
    UUID(..),
    mkUUID,
    unUUID
    ) where

import Control.Exception.Base (assert)

newtype UUID = MkUUID Int
    deriving (Show, Read, Eq, Num, Ord)

{-# INLINABLE mkUUID #-}
mkUUID :: Int -> UUID
mkUUID i = assert (i >= 0) (MkUUID i)

{-# INLINABLE unUUID #-}
unUUID :: UUID -> Int
unUUID (MkUUID i) = i
