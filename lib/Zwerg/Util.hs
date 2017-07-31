module Zwerg.Util where

import Zwerg.Prelude
import Paths_zwerg (getDataFileName)

import Control.Exception.Base (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T (replicate, append, length)

getAsset :: MonadIO m => Text -> m Text
getAsset path = pack <$> liftIO (getDataFileName $ "assets/" ++ unpack path)

assertfM :: Monad m => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

{-# INLINABLE whenJust #-}
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

{-# INLINABLE whenJustErr #-}
whenJustErr :: (MonadError ZError m) => Maybe a -> ZError -> (a -> m b) -> m b
whenJustErr mg err f = maybe (throwError err) f mg

{-# INLINABLE whenJustM #-}
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

{-# INLINABLE fromJustErrM #-}
fromJustErrM :: (MonadError ZError m) => Maybe a -> ZError -> m a
fromJustErrM x err = maybe (throwError err) return x

{-# INLINABLE whenM #-}
whenM :: Monad m => m Bool -> m () -> m ()
whenM mg f = mg >>= (flip when) f

{-# INLINABLE chunksOf #-}
chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    build g = g (:) []
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n

{-# INLINABLE takeWhileM1 #-}
takeWhileM1 :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM1 _ [] = return []
takeWhileM1 p (x:xs) = do
  q <- p x
  if q then liftM ((:) x) (takeWhileM1 p xs)
       else return [x]

{-# INLINABLE leftPad #-}
leftPad :: Int -> Text -> Text
leftPad n t =
  let tlen = T.length t
  in if tlen < n then T.append (T.replicate (n - tlen) " ") t else t

