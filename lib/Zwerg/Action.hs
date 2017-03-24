module Zwerg.Action (Action(..)) where

import Zwerg.Prelude
import Zwerg.Component.UUID (UUID)

import Data.Text (Text)

data Action =
    Damage
    { targetUUID     :: UUID
    , amount         :: Int }
    | MeleeAttack
    { attackerUUID   :: UUID
    , defenderUUID   :: UUID }
    | Defence
    { attackerUUID   :: UUID
    , defenderUUID   :: UUID }
    | Dying
    { dyingUUID      :: UUID }
    | Dead
    { deadUUID       :: UUID }
    | GenerateEntity
    { onTile         :: UUID
    , entityTextID   :: Text }
    deriving (Show,Read,Eq,Ord)

-- newtype MessageQueue = MkMessageQueue (Seq Message)
--     deriving (
--         Monoid,
--         Show,
--         Read,
--         Eq,
--         Ord
--     )
--
-- class HasMessageQueue s where
--     msgQueue :: Lens' s MessageQueue
--
-- {-# INLINABLE emptyMsgQueue #-}
-- emptyMsgQueue :: MessageQueue
-- emptyMsgQueue = MkMessageQueue S.empty
--
-- {-# INLINABLE pushMsg #-}
-- -- | Pushes a message to the back of the queue.
-- pushMsg :: Message -> MessageQueue -> MessageQueue
-- pushMsg msg (MkMessageQueue queue) = MkMessageQueue $ queue S.|> msg
--
-- {-# INLINABLE pushMsgs #-}
-- -- | Pushes a message to the back of the queue.
-- pushMsgs :: MessageQueue -> MessageQueue -> MessageQueue
-- pushMsgs (MkMessageQueue q1) (MkMessageQueue q2) = MkMessageQueue $ q1 S.>< q2
--
-- {-# INLINABLE popMsg #-}
-- -- | Pops a message off the front of the queue for Systems to process
-- -- returns Nothing if the message queue is empty
-- popMsg :: MessageQueue -> (Maybe Message, MessageQueue)
-- popMsg (MkMessageQueue queue) =
--     case S.viewl queue of
--       S.EmptyL -> (Nothing, MkMessageQueue S.empty)
--       m S.:< ms -> (Just m, MkMessageQueue ms)
