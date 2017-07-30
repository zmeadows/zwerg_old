module Zwerg.Data.ZError
  ( ZErrorLevel(..)
  , ZError(..)
  , ZConstructable(..)
  , throw
  , maybeThrow
  ) where

import Prelude (Show, Ord, Eq, Int, ($))
import Data.Text (Text)
import Lens.Micro.Platform (makeClassy)
import Language.Haskell.TH (Exp, runQ)
import Language.Haskell.TH.Syntax (Quasi)
import Control.Monad.Except (MonadError, throwError)

data ZErrorLevel = PlayerWarning | EngineWarning | EngineFatal
  deriving (Show, Eq, Ord)

data ZError = ZError
  { _file :: Text
  , _line :: Int
  , _errLevel :: ZErrorLevel
  , _explanation :: Text
  } deriving (Show, Eq)
makeClassy ''ZError

throw :: Language.Haskell.TH.Syntax.Quasi m => m Exp
throw = runQ [| \l d -> throwError $ ZError __FILE__ __LINE__ l d|]

maybeThrow :: Language.Haskell.TH.Syntax.Quasi m => m Exp
maybeThrow = runQ [| \l d x -> maybe (throwError $ ZError __FILE__ __LINE__ l d) return x|]

class ZConstructable a b | a -> b where
  zConstruct :: (MonadError ZError m) => b -> m a


