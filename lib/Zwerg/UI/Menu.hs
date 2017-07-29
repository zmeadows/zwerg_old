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
import qualified Data.Sequence as S (viewr, viewl, empty, fromList, length)
import Data.Text (Text)
import qualified Data.Text as T

import Unsafe (unsafeHead, unsafeTail)

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

next :: Menu a -> Menu a
next m@(MkMenu ls x rs) =
  case S.viewl rs of
    EmptyL ->
      case S.viewl ls of
        EmptyL -> m
        a :< ls' -> MkMenu S.empty a (ls' |> x)
    a :< rs' -> MkMenu (ls |> x) a rs'

prev :: Menu a -> Menu a
prev m@(MkMenu ls x rs) =
  case S.viewr ls of
    EmptyR ->
      case S.viewr rs of
        EmptyR -> m
        rs' :> a -> MkMenu (x <| rs') a S.empty
    ls' :> a -> MkMenu ls' a (x <| rs)

--  if | S.null ls && S.null rs -> m
--     | S.null ls && not (S.null rs) ->
--       let (rs' :> a) = S.viewr rs
--       in MkMenu (x <| rs') a S.empty
--     | otherwise ->
--       let (ls' :> a) = S.viewr ls
--       in MkMenu ls' a (x <| rs)

focus :: Menu a -> MenuEntry a
focus (MkMenu _ x _) = x

makeMenu :: NonEmpty (Text, a) -> Menu a
makeMenu entries = makeMenu' entries []

makeMenu' :: NonEmpty (Text, a) -> [MenuEntry a] -> Menu a
makeMenu' ((newLabel, newItem) :| remaining) entries =
  let alreadyUsedChars = map (view shortcut) entries :: [Char]
      makeCharPair ch = (ch, length $ filter (== ch) alreadyUsedChars)
      newCharCounts = map makeCharPair $ T.unpack newLabel
      newChar = fst $ minimumBy (compare `on` snd) newCharCounts
      newEntries = MenuEntry newChar newLabel newItem : entries
  in case remaining of
       r:rs -> makeMenu' (r :| rs) newEntries
       [] -> let res = reverse newEntries
             in MkMenu S.empty (unsafeHead res) (S.fromList $ unsafeTail res)

getMenuLabels :: Menu a -> [Text]
getMenuLabels (MkMenu ls x rs) =
  let leftLabels = fmap (view label) ls
      rightLabels = fmap (view label) rs
      focusLabel = view label x
  in toList $ (leftLabels |> focusLabel) >< rightLabels

getMenuFocusIndex :: Menu a -> Int
getMenuFocusIndex (MkMenu ls _ _) = S.length ls
