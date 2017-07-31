module Zwerg.Data.ZError
  ( ZErrorLevel(..)
  , ZError
  , ZConstructable(..)
  , HasZError(..)
  , throw
  , maybeThrow
  ) where

import Prelude (Show, Ord, Eq, ($))
import Data.Text (Text)
import Lens.Micro.Platform (makeClassy)
import Language.Haskell.TH (Exp, runQ)
import Language.Haskell.TH.Syntax (Quasi)
import Control.Monad.Except (MonadError, throwError)
import GHC.Stack as EXPORTED (CallStack, callStack)


data ZErrorLevel = PlayerWarning | EngineWarning | EngineFatal
  deriving (Show, Eq, Ord)

data ZError = ZError
  { _errLevel :: ZErrorLevel
  , _stack :: CallStack
  , _explanation :: Text
  } deriving (Show)
makeClassy ''ZError

throw :: Language.Haskell.TH.Syntax.Quasi m => m Exp
throw = runQ [| \l d -> throwError $ ZError l callStack d|]

maybeThrow :: Language.Haskell.TH.Syntax.Quasi m => m Exp
maybeThrow = runQ [| \l d x -> maybe (throwError $ ZError l callStack d) return x|]

class ZConstructable a b | a -> b where
  zConstruct :: (MonadError ZError m) => b -> m a


