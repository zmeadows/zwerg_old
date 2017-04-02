module Zwerg.UI.Menu where

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
  } deriving (Show, Eq, Functor)

makeLenses ''MenuEntry

data Menu a =
  MkMenu (Seq (MenuEntry a))
         (MenuEntry a)
         (Seq (MenuEntry a))
  deriving (Show, Eq, Functor)

type TextMenu = Menu ()

instance ZConstructable (Menu a) [(Text, a)] where
  zConstruct xs =
    if | null xs ->
         throwError $
         ZError __FILE__ __LINE__ Fatal "Tried to construct empty menu"
       | otherwise -> return $ makeMenu xs

cycleMenu :: Menu a -> Menu a
cycleMenu (MkMenu ls x rs) = MkMenu rs x ls

next :: Menu a -> Menu a
next m@(MkMenu ls x rs) =
  if | S.null rs && S.null ls -> m
     | S.null rs && (not $ S.null ls) -> next $ cycleMenu m
     | otherwise ->
       let (a :< rs') = S.viewl rs
       in MkMenu (ls |> x) a rs'

prev :: Menu a -> Menu a
prev m@(MkMenu ls x rs) =
  if | S.null ls && S.null rs -> m
     | S.null ls && (not $ S.null rs) -> prev $ cycleMenu m
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
  in makeMenu' remaining $ (MenuEntry newChar newLabel newItem) : entries
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
