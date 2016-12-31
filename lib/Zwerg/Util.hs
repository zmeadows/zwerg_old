module Zwerg.Util where

import Paths_zwerg (getDataFileName)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Base (assert)

getAsset :: MonadIO m => FilePath -> m FilePath
getAsset path = liftIO (getDataFileName $ "assets/" ++ path)

assertfM :: Monad m => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())

-- | Perform some operation on 'Just', given the field inside the 'Just'.
--
-- > whenJust Nothing  print == return ()
-- > whenJust (Just 1) print == print 1
whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (pure ()) f mg

-- | Like 'whenJust', but where the test can be monadic.
whenJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whenJustM mg f = maybe (return ()) f =<< mg
