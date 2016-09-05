module Zwerg.System where

import Zwerg.Event (Event)
import Zwerg.Game  (HasGameState(..))

import Control.Monad.State (MonadState)

newtype System = MkSystem
    { runSystem :: forall s m. (HasGameState s, MonadState s m)
                => Event -> m () }
