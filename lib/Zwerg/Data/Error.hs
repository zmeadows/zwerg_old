module Zwerg.Data.Error where

data Error = PathBlocked | OutOfAmmo deriving (Show,Read,Eq)
