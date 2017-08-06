module Zwerg.Util where

import Zwerg.Prelude
import Paths_zwerg (getDataFileName)

import Control.Exception.Base (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T (replicate, append, length)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

getAsset :: MonadIO m => Text -> m Text
getAsset path = pack <$> liftIO (getDataFileName $ "assets/" ++ unpack path)

assertfM :: Monad m => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

whenM :: Monad m => m Bool -> m () -> m ()
whenM mg f = mg >>= (flip when) f

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    build g = g (:) []
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

takeWhileM1 :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM1 _ [] = return []
takeWhileM1 p (x:xs) = do
  q <- p x
  if q then liftM ((:) x) (takeWhileM1 p xs)
       else return [x]

leftPad :: Int -> Text -> Text
leftPad n t =
  let tlen = T.length t
  in if tlen < n then T.append (T.replicate (n - tlen) " ") t else t

type MComparator m a = a -> a -> m Ordering

sortByM :: (Monad m, Functor m) => MComparator m a -> [a] -> m [a]
sortByM _ []  = return []
sortByM _ [x] = return [x]
sortByM cmp xs = do
  let (ys, zs) = partition xs
  ys' <- sortByM cmp ys
  zs' <- sortByM cmp zs
  merge ys' zs'
  where merge [] bs = return bs
        merge as [] = return as
        merge (a:as) (b:bs) = do
          comparison <- cmp a b
          case comparison of
            LT -> (a:) <$> merge as (b:bs)
            _  -> (b:) <$> merge (a:as) bs
        partition ls = splitAt (length ls `quot` 2) ls
