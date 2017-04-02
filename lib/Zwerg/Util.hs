module Zwerg.Util where

import Zwerg.Data.Error
import Zwerg.Prelude

import Control.Exception.Base (assert)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (unpack, pack)
import Paths_zwerg (getDataFileName)

getAsset
  :: MonadIO m
  => Text -> m Text
getAsset path = pack <$> liftIO (getDataFileName $ "assets/" ++ unpack path)

assertfM
  :: Monad m
  => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust
  :: Applicative m
  => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenJustErr
  :: (MonadError ZError m)
  => Maybe a -> ZError -> (a -> m b) -> m b
whenJustErr mg err f = maybe (throwError err) f mg

-- | Like 'whenJust', but where the test can be monadic.
whenJustM
  :: Monad m
  => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

fromJustErrM
  :: (MonadError ZError m)
  => Maybe a -> ZError -> m a
fromJustErrM x err = maybe (throwError err) return x

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n = l `c` splitter (drop i l) c n
