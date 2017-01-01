module Zwerg.UI.Menu where

import Data.List.Zipper (Zipper)
import qualified Data.List.Zipper as Z

newtype Menu a = MkMenu (Zipper a) 
    deriving (Show, Eq)

makeMenu :: [a] -> Menu a
makeMenu = MkMenu . Z.fromList

next :: Menu a -> Menu a
next (MkMenu m) = MkMenu $ Z.right m

prev :: Menu a -> Menu a
prev (MkMenu m) = MkMenu $ Z.left m

focus :: Menu a -> a
focus (MkMenu m) = Z.cursor m
