{-# LANGUAGE CPP, OverloadedStrings #-}
module Zwerg.Entity.Player where

import Zwerg.Entity
import Zwerg.Types

import Data.Text(Text)
import Control.Monad (when)
import Data.Maybe
import Control.Monad.Except (throwError)
import Data.Label.Monadic

import qualified Data.IntMap as I

-- TODO: check that position inserted isn't already occupied
makePlayer :: Text -> Position -> Int -> Int -> System Int
makePlayer nameStr pos initHP layerUUID = do
    tileUUID <- tileUUIDatPos layerUUID pos
    when (isNothing tileUUID) $ throwError $
      FatalError __LINE__ "Entity.makePlayer: attempted to create player on non-existent tile"
    uuid       <- gets nextUUID
    playerUUID =: uuid
    name       =. I.insert uuid nameStr
    position   =. I.insert uuid pos
    hp         =. I.insert uuid initHP
    entityType =. I.insert uuid Player
    glyph      =. I.insert uuid ('@', Attributes FontRegular White Black 0 0)
    items      =. I.insert uuid []
    layer      =. I.insert uuid layerUUID
    occupants  =. I.adjust (uuid :) (fromJust tileUUID)
    nextUUID   =. (+ 1)
    return uuid

