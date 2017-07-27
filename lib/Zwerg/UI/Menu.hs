module Zwerg.UI.Menu (
  MenuEntry,
  Menu,
  next,
  prev,
  makeMenu,
  label,
  shortcut,
  item,
  focus,
  getMenuLabels,
  getMenuFocusIndex,
  TextMenu
  ) where

import Zwerg.Prelude

import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Sequence (Seq, (<|), (|>), ViewL(..), ViewR(..), (><))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T

data MenuEntry a = MenuEntry
  { _shortcut :: Char
  , _label :: Text
  , _item :: a
  } deriving (Show, Eq, Functor, Generic)

makeLenses ''MenuEntry
instance Binary a => Binary (MenuEntry a)

data Menu a =
  MkMenu (Seq (MenuEntry a))
         (MenuEntry a)
         (Seq (MenuEntry a))
  deriving (Show, Eq, Functor, Generic)

type TextMenu = Menu ()

instance Binary a => Binary (Menu a)

instance ZConstructable (Menu a) [(Text, a)] where
  zConstruct xs =
    if | null xs -> $(throw) EngineFatal "Tried to construct empty menu"
       | otherwise -> return $ makeMenu xs

next :: Menu a -> Menu a
next m@(MkMenu ls x rs) =
  if | S.null rs && S.null ls -> m
     | S.null rs && not (S.null ls) ->
        let (a :< ls') = S.viewl ls
        in MkMenu S.empty a (ls' |> x)
     | otherwise ->
       let (a :< rs') = S.viewl rs
       in MkMenu (ls |> x) a rs'

prev :: Menu a -> Menu a
prev m@(MkMenu ls x rs) =
  if | S.null ls && S.null rs -> m
     | S.null ls && not (S.null rs) ->
       let (rs' :> a) = S.viewr rs
       in MkMenu (x <| rs') a S.empty
     | otherwise ->
       let (ls' :> a) = S.viewr ls
       in MkMenu ls' a (x <| rs)

focus :: Menu a -> MenuEntry a
focus (MkMenu _ x _) = x

makeMenu :: [(Text, a)] -> Menu a
makeMenu entries = makeMenu' entries []

makeMenu' :: [(Text, a)] -> [MenuEntry a] -> Menu a
makeMenu' ((newLabel, newItem):remaining) entries =
  let alreadyUsedChars = map (view shortcut) entries :: [Char]
      newCharCounts =
        map
          (\x -> (x, length $ filter (== x) alreadyUsedChars))
          (T.unpack newLabel)
      newChar = fst $ minimumBy (compare `on` snd) newCharCounts
  in makeMenu' remaining $ MenuEntry newChar newLabel newItem : entries
makeMenu' [] entries =
  let e:es = reverse entries
  in MkMenu S.empty e (S.fromList es)

getMenuLabels :: Menu a -> [Text]
getMenuLabels (MkMenu ls x rs) =
  let leftLabels = fmap (view label) ls
      rightLabels = fmap (view label) rs
      focusLabel = view label x
  in toList $ (leftLabels |> focusLabel) >< rightLabels

getMenuFocusIndex :: Menu a -> Int
getMenuFocusIndex (MkMenu ls _ _) = S.length ls
