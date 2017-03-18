module Zwerg.Generator (
    module EXPORTED,
    Generator(..),
    GeneratorState(..),
    HasGeneratorState(..),
    initGeneratorState
    ) where

import Zwerg.Component       as EXPORTED
import Zwerg.Behavior        as EXPORTED
import Zwerg.Component.UUID  as EXPORTED
import Zwerg.Component.Glyph as EXPORTED
import Zwerg.Data.Color      as EXPORTED
import Zwerg.Data.UUIDGen    as EXPORTED
import Zwerg.Data.UUIDSet    as EXPORTED
import Zwerg.Data.RanGen     as EXPORTED

import Control.Monad.Random       as EXPORTED (RandT, MonadRandom, evalRandT)
import Control.Monad.State.Strict as EXPORTED (State, MonadState, execState)

import Control.Lens.TH (makeClassy)

data GeneratorState = GeneratorState
    { _genComponents :: Components
    , _genBehaviors  :: Behaviors
    , _genUUIDGen    :: UUIDGen
    , _targetUUID   :: UUID
    } deriving (Show)
makeClassy ''GeneratorState

instance HasComponents GeneratorState where
    components = genComponents

instance HasBehaviors GeneratorState where
    behaviors = genBehaviors

instance HasUUIDGen GeneratorState where
    uuidGen = genUUIDGen

initGeneratorState :: UUIDGen -> GeneratorState
initGeneratorState ug =
    let (i, ug') = popUUID ug
    in GeneratorState
    { _genComponents = emptyComponents
    , _genBehaviors  = emptyBehaviors
    , _genUUIDGen    = ug'
    , _targetUUID    = i
    }

newtype Generator a =
    Generator (RandT RanGen (State GeneratorState) a)
        deriving (
            Functor,
            Applicative,
            Monad,
            MonadState GeneratorState,
            MonadRandom
        )

-- generateWith :: RanGen
--              -> UUIDGen
--              -> Generator ()
--              -> GeneratorState
-- generateWith rg ug (Generator a) =
--     execState (evalRandT a rg) (initGeneratorState ug)

