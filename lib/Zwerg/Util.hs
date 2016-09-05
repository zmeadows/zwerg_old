module Zwerg.Util where

import Paths_zwerg (getDataFileName)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Base (assert)

getAsset :: MonadIO m => FilePath -> m FilePath
getAsset path = liftIO (getDataFileName $ "assets/" ++ path)

assertfM :: Monad m => a -> (a -> m Bool) -> m ()
assertfM x f = f x >>= flip assert (return ())
