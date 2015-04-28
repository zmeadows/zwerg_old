{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Zwerg.Core where

import Zwerg.Types
import Zwerg.Entity
import Zwerg.Port

import Data.Label
import Data.Label.Monadic
import Control.Monad.State.Strict hiding (gets)

import Data.Text (Text)
import Data.Text as T (unpack)
import System.Random.Mersenne.Pure64

import Data.Map.Strict as M

fclabels [d|
  data ZWERGState = ZWERGState {
      entities    :: Entities,
      port        :: Port,
      timeElapsed :: Double,
      ticksElapsed       :: Int,
      messages    :: [(Text, Color)],
      glyphMap    :: Map Position Glyph,
      randGen     :: PureMT
      } deriving (Show)
  |]

type ZWERG = State ZWERGState

_INITZWERGSTATE :: ZWERGState
_INITZWERGSTATE = ZWERGState emptyEntities mainMenu 0.0 0 [] M.empty (pureMT 0)

clearMessages :: ZWERG ()
clearMessages = messages =: []

modifyEntities :: System Port -> ZWERG ()
modifyEntities sys = do
    oldGen <- gets randGen
    oldEntities <- gets entities
    let (result, newGen, newEntities) = runSystem (returnWithGlyphs sys) oldGen oldEntities
    randGen =: newGen
    case result of
      Right (newPort, ug) -> do
          entities =: newEntities
          port =: newPort
          glyphMap =. M.union (M.fromList ug)
      Left (PlayerError txt) -> messages =. ((txt, Red) :)
      Left (FatalError ln txt) ->  error (T.unpack txt ++ " LINE NUMBER: " ++ show ln)

returnWithGlyphs :: System a -> System (a, [(Position, Glyph)])
returnWithGlyphs sys = do
    res <- sys
    uuid <- gets playerUUID
    if (uuid /= (-1))
      then do
        layerUUID <- unsafeLookupComp uuid layer
        ug <- getUpdatedGlyphs layerUUID
        return (res,ug)
      else (return (res, []))


