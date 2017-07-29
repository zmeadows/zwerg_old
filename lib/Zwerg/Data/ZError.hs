module Zwerg.Data.ZError
  ( ZErrorLevel(..)
  , ZError(..)
  , ZConstructable(..)
  , throw
  , maybeThrow
  ) where

import Protolude (Show, Ord, Eq, Text, Int, MonadError, ($))
import Lens.Micro.Platform (makeClassy)
import Language.Haskell.TH (Exp, runQ)
import Language.Haskell.TH.Syntax (Quasi)

data ZErrorLevel = PlayerWarning | EngineWarning | EngineFatal
  deriving (Show, Eq, Ord)

data ZError = ZError
  { _file :: Text
  , _line :: Int
  , _errLevel :: ZErrorLevel
  , _explanation :: Text
  } deriving (Show, Eq)
makeClassy ''ZError

{-
printZError :: ZError -> Text
printZError zerr = T.concat [
    "File: ", zerr ^. file, "\n",
    "Line: ", show $ zerr ^. line, "\n",
    "ErrorLevel: ", show $ zerr ^. errLevel, "\n",
    "Description: ", show $ zerr ^. description, "\n"
  ]
-}

throw :: Language.Haskell.TH.Syntax.Quasi m => m Exp
throw = runQ [| \l d -> throwError $ ZError __FILE__ __LINE__ l d|]

maybeThrow :: Language.Haskell.TH.Syntax.Quasi m => m Exp
maybeThrow = runQ [| \l d x -> maybe (throwError $ ZError __FILE__ __LINE__ l d) return x|]


class ZConstructable a b | a -> b where
  zConstruct :: (MonadError ZError m) => b -> m a


