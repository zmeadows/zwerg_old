module Zwerg.Animation where

import Zwerg.Prelude
import Zwerg.Component.Position (Position)
import Zwerg.Data.Color (Color)

import Data.Sequence (Seq)
import qualified Data.Sequence as S

import Control.Lens (Lens')

data Animation =
    SmallBeam
    { startPosition :: Position
    , endPosition   :: Position
    , zcolor        :: Color }
    | Blink
    { position      :: Position
    , zcolor        :: Color }
    deriving (Show, Read, Eq)

newtype AnimationQueue = MkAnimationQueue (Seq Animation)
    deriving (
        Monoid,
        Show,
        Read,
        Eq
    )

class HasAnimationQueue s where
    animationQueue :: Lens' s AnimationQueue

{-# INLINABLE emptyAnimationQueue #-}
emptyAnimationQueue :: AnimationQueue
emptyAnimationQueue = MkAnimationQueue S.empty

{-# INLINABLE pushAnimation #-}
pushAnimation :: Animation -> AnimationQueue -> AnimationQueue
pushAnimation anim (MkAnimationQueue queue) = MkAnimationQueue $ queue S.|> anim
