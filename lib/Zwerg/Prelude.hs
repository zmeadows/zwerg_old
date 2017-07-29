module Zwerg.Prelude (String, module EXPORTED) where

import Protolude as EXPORTED hiding (to, forM, (<>))

import Zwerg.Prelude.Class as EXPORTED
import Zwerg.Prelude.Primitives as EXPORTED
import Zwerg.Data.ZError as EXPORTED

import Control.Monad.Random.Class as EXPORTED
import Data.Binary as EXPORTED (Binary)
import Data.Monoid as EXPORTED ((<>))
import Data.Text as EXPORTED (Text, pack, unpack)
import Data.Traversable as EXPORTED (forM)
import GHC.Generics as EXPORTED (Generic)

import Lens.Micro.Internal as EXPORTED (At(..), Ixed(..), Index, IxValue)
import Lens.Micro.Platform as EXPORTED
       (makeClassy, makeLenses, makeFields, (%=), (^.), (.=), over, use,
        view, to, set, Lens', (<&>))

import Data.List.NonEmpty as EXPORTED (NonEmpty(..))

import Data.Semigroup as EXPORTED (Semigroup)

type String = [Char]
