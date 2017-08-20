module Zwerg.UI.Menu
    ( MenuEntry
    , Menu
    , MenuGroupSelect
    , TextMenu
    , toggleFocus
    , getAllSelected
    , isEntryMarked
    , makeMenuGroupSelect
    , next
    , prev
    , makeMenu
    , label
    , shortcut
    , item
    , focus
    , getMenuLabels
    , getMenuFocusIndex
    ) where

import Zwerg.Prelude

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Sequence (Seq, (<|), (|>), ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as S (viewr, viewl, empty, fromList, length, filter, singleton)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE (map)

data MenuEntry a = MenuEntry
    { shortcut :: Char
    , label    :: Text
    , item     :: a }
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass Binary

data Menu a = MkMenu (Seq (MenuEntry a)) (MenuEntry a) (Seq (MenuEntry a))
  deriving stock (Functor, Foldable, Traversable, Generic)
  deriving anyclass Binary

type TextMenu = Menu ()

next :: Menu a -> Menu a
next m@(MkMenu ls x rs) =
  case S.viewl rs of
    EmptyL -> case S.viewl ls of
                EmptyL -> m
                a :< ls' -> MkMenu S.empty a (ls' |> x)
    a :< rs' -> MkMenu (ls |> x) a rs'

prev :: Menu a -> Menu a
prev m@(MkMenu ls x rs) =
  case S.viewr ls of
    EmptyR -> case S.viewr rs of
                EmptyR -> m
                rs' :> a -> MkMenu (x <| rs') a S.empty
    ls' :> a -> MkMenu ls' a (x <| rs)

focus :: Menu a -> MenuEntry a
focus (MkMenu _ x _) = x

makeMenu :: NonEmpty (Text, a) -> Menu a
makeMenu entries = makeMenu' entries []

makeMenu' :: NonEmpty (Text, a) -> [MenuEntry a] -> Menu a
makeMenu' ((newLabel, newItem) :| remaining) entries =
  let alreadyUsedChars = map shortcut entries
      makeCharPair ch = (ch, length $ filter (== ch) alreadyUsedChars)
      newCharCounts = map makeCharPair $ T.unpack newLabel
      newChar = fst $ minimumBy (compare `on` snd) newCharCounts
      newEntries = MenuEntry newChar newLabel newItem : entries
  in case remaining of
       r:rs -> makeMenu' (r :| rs) newEntries
       [] -> let res = reverse newEntries
             in MkMenu S.empty (head res) (S.fromList $ tail res)

getMenuLabels :: Menu a -> [Text]
getMenuLabels (MkMenu ls x rs) =
  let leftLabels = fmap label ls
      rightLabels = fmap label rs
      focusLabel = label x
  in toList $ (leftLabels |> focusLabel) >< rightLabels

getMenuFocusIndex :: Menu a -> Int
getMenuFocusIndex (MkMenu ls _ _) = S.length ls

type MenuGroupSelect a = Menu (a, Bool)
type MenuGroupSelectEntry a = MenuEntry (a, Bool)

isEntryMarked :: MenuGroupSelectEntry a -> Bool
isEntryMarked = snd . item

toggleEntryMarked :: MenuGroupSelectEntry a -> MenuGroupSelectEntry a
toggleEntryMarked m = m { item = (item', not isSelected) }
    where (item', isSelected) = item m

makeMenuGroupSelect :: NonEmpty (Text, a) -> MenuGroupSelect a
makeMenuGroupSelect entries = makeMenu' unmarkedEntries []
  where unmarkedEntries = NE.map (\(name,val) -> (name, (val, False))) entries

toggleFocus :: MenuGroupSelect a -> MenuGroupSelect a
toggleFocus (MkMenu ls x rs) =
    if (snd $ item x)
        then MkMenu ls (toggleEntryMarked x) rs
        else MkMenu ls (toggleEntryMarked x) rs

getAllSelected :: MenuGroupSelect a -> [a]
getAllSelected (MkMenu ls x rs) = toList $ (getMarked ls) >< focusMarked >< (getMarked rs)
    where getMarked s = fmap (fst . item) $ S.filter (snd . item) $ s
          focusMarked = if (snd $ item x) then S.singleton (fst $ item x) else S.empty
