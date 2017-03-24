module Zwerg.UI.Menu where

import Zwerg.Prelude
import Zwerg.Class
import Zwerg.Data.Error

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Sequence (Seq, (<|), (|>), ViewL(..), ViewR(..))
import qualified Data.Sequence as S
import Control.Lens (makeLenses, view)

data MenuEntry a = MenuEntry
    { _shortcut :: Char
    , _label    :: Text
    , _item     :: a
    } deriving (Show, Eq)
makeLenses ''MenuEntry

newtype Menu a = MkMenu (Seq (MenuEntry a))
  deriving (Show, Eq)

type TextMenu = Menu ()

instance ZConstructable (Menu a) [(Text,a)] where
  zConstruct xs = if
    | null xs -> throwError $ ZError __FILE__ __LINE__ Fatal "Tried to construct empty menu"
    | otherwise -> return $ makeMenu xs

next :: Menu a -> Menu a
next (MkMenu s) = if
  | S.null s        -> MkMenu S.empty
  | S.length s == 1 -> MkMenu s
  | otherwise       -> let (a :< s') = S.viewl s in MkMenu $ s' |> a

prev :: Menu a -> Menu a
prev (MkMenu s) = if
  | S.null s        -> MkMenu S.empty
  | S.length s == 1 -> MkMenu s
  | otherwise       -> let (s' :> a) = S.viewr s in MkMenu $ a <| s'

focus :: Menu a -> MenuEntry a
focus (MkMenu s) = let (a :< _) = S.viewl s in a

makeMenu :: [(Text,a)] -> Menu a
makeMenu entries = makeMenu' entries []

makeMenu' :: [(Text,a)] -> [MenuEntry a] -> Menu a
makeMenu' ((newLabel, newItem) : remaining) entries =
  let alreadyUsedChars = map (view shortcut) entries :: [Char]
      newCharCounts = map (\x -> (x, length $ filter (== x) alreadyUsedChars)) (T.unpack newLabel)
      newChar = fst $ minimumBy (compare `on` snd) newCharCounts
  in makeMenu' remaining $ (MenuEntry newChar newLabel newItem):entries

makeMenu' [] entries = MkMenu $ S.fromList entries

getMenuLabels :: Menu a -> [Text]
getMenuLabels (MkMenu s) = toList $ fmap (view label) s
