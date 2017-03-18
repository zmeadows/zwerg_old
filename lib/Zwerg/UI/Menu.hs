module Zwerg.UI.Menu where

import Data.Text (Text, unpack)
import Data.Foldable (minimumBy)
import Data.Function (on)

data MenuEntry a = MenuEntry
    { shortcut :: Char
    , label    :: Text
    , item     :: a
    } deriving (Show, Eq)

data Menu a = Menu [MenuEntry a] [MenuEntry a] deriving (Show, Eq)

type TextMenu = Menu ()

next :: Menu a -> Menu a
next (Menu [] [r]) = (Menu [] [r])
next (Menu [] (r:rs)) = (Menu [r] rs)
next (Menu ls [r]) = Menu [] (ls ++ [r])
next (Menu ls (r:rs)) = Menu (ls ++ [r]) rs

prev :: Menu a -> Menu a
prev (Menu [] [r]) = (Menu [] [r])
prev (Menu [] rs) = (Menu (init rs) [last rs])
prev (Menu ls rs) = Menu (init ls) ((last ls):rs)

focus :: Menu a -> MenuEntry a
focus (Menu _ (r:_)) = r
focus (Menu _ []) = error $ "blah"

makeTextMenu :: [Text] -> TextMenu
makeTextMenu labels = makeTextMenu' labels []

makeTextMenu' :: [Text] -> [MenuEntry ()] -> TextMenu
makeTextMenu' (l:ls) entries =
  let usedChars = map shortcut entries
      newCharCounts = zip (unpack l) $ map (\x -> length $ filter (== x) usedChars) $ unpack l
      newChar = fst $ minimumBy (compare `on` snd) newCharCounts
  in makeTextMenu' ls $ (MenuEntry newChar l ()):entries
makeTextMenu' [] entries = Menu [] $ reverse entries

menuToList :: Menu a -> [Text]
menuToList (Menu ls rs) = map label (ls ++ rs)
