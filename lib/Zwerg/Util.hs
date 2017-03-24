module Zwerg.Util where

import Zwerg.Prelude
import Zwerg.Data.Error

import Paths_zwerg (getDataFileName)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Base (assert)
import Data.Text (unpack, pack)

getAsset :: MonadIO m => Text -> m Text
getAsset path = pack <$> liftIO (getDataFileName $ "assets/" ++ unpack path)

assertfM :: Monad m => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

whenJustErr :: (MonadError ZError m) => Maybe a -> ZError -> (a -> m b) -> m b
whenJustErr mg err f = maybe (throwError err) f mg

-- | Like 'whenJust', but where the test can be monadic.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg

fromJustErrM :: (MonadError ZError m) => Maybe a -> ZError -> m a
fromJustErrM x err = maybe (throwError err) return x

