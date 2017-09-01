module Zwerg.Util where

import Zwerg.Prelude

import Data.Text (Text)
import qualified Data.Text as T (replicate, append, length)

{-# INLINABLE condM #-}
condM :: Monad m => [(m Bool, m ())] -> m ()
condM [] = return ()
condM ((f1,f2):rest) = f1 >>= \b -> if b then f2  else condM rest

{-# INLINABLE takeUntil #-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

{-# INLINABLE whenJust #-}
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

{-# INLINABLE whenJustM #-}
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

{-# INLINABLE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM mg f = mg >>= (flip when) f

{-# INLINABLE inM2 #-}
inM2 :: Monad m => (a -> m b) -> (a,a) -> m (b,b)
inM2 f t = sequenceT $ join (***) f t

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
