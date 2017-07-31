module Zwerg.Prelude
  ( show
  , module EXPORTED
  ) where

import Prelude as EXPORTED hiding (show, Monoid(..))
import qualified Prelude as P (show)

import Zwerg.Prelude.Class as EXPORTED
import Zwerg.Prelude.Primitives as EXPORTED
import Zwerg.Data.ZError as EXPORTED

import Control.Monad.Except as EXPORTED
import Control.Monad.Random.Class as EXPORTED hiding (fromList)
import Control.Monad.Reader as EXPORTED
import Control.Monad.State.Strict as EXPORTED

import GHC.Stack as EXPORTED (HasCallStack, CallStack, callStack, prettyCallStack)

import Data.Binary as EXPORTED (Binary)
import Data.List.NonEmpty as EXPORTED (NonEmpty(..))
import Data.Semigroup as EXPORTED
import Data.String.Conv (StringConv, toS)
import Data.Text as EXPORTED (Text, pack, unpack)
import Data.Traversable as EXPORTED (forM)

import GHC.Exts as EXPORTED (IsList(..))
import GHC.Generics as EXPORTED (Generic)

import Lens.Micro.Internal as EXPORTED (At(..), Ixed(..), Index, IxValue)
import Lens.Micro.Platform as EXPORTED
       (makeClassy, makeLenses, makeFields, (%=), (^.), (.=), over, use,
        view, to, set, Lens, Lens', (<&>))

{-# SPECIALIZE show :: Show a => a -> Text  #-}
{-# SPECIALIZE show :: Show a => a -> String  #-}
show :: (Show a, StringConv String b) => a -> b
show x = toS (P.show x)
