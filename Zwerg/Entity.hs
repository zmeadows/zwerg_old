{-# LANGUAGE CPP, TemplateHaskell, TypeOperators, OverloadedStrings,
    GeneralizedNewtypeDeriving, TupleSections #-}
module Zwerg.Entity where

import Zwerg.Types

import Prelude hiding ((.))
import Control.Category ((.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Text (Text)
import Data.Label
import Data.Label.Monadic
import Control.Monad.Except

import Control.Monad.Random(RandT, MonadRandom, runRandT)
import System.Random.Mersenne.Pure64

import Data.List as L
import Data.Ord(comparing)

import Control.Monad.State.Strict hiding (gets)

import Data.Maybe

-- EntityType Ord instance for glyph drawing priority
data EntityType = Player | Enemy | Item | Tile | Layer deriving (Show, Read, Eq, Ord)

fclabels [d|
  data Entities = Entities {
      name        :: IntMap Text,
      description :: IntMap Text,
      entityType  :: IntMap EntityType,
      position    :: IntMap Position,
      depth       :: IntMap Int,
      hp          :: IntMap Int,
      move        :: IntMap Direction,
      tileType    :: IntMap TileType,
      glyph       :: IntMap Glyph,
      items       :: IntMap [Int],
      occupants   :: IntMap [Int],
      layer       :: IntMap Int,
      tiles       :: IntMap [Int],
      width       :: IntMap Int,
      height      :: IntMap Int,
      needsRedraw :: IntMap Bool,
      blocked     :: IntMap Bool,
      nextUUID    :: Int,
      playerUUID  :: Int
      -- animations, hits, stats, equipment, status effects, descriptions, needRedraw, timeTillTurn
  } deriving (Show, Read, Eq)
  |]

type Component a = Entities :-> IntMap a

newtype System a = System (ExceptT ZWERGError (RandT PureMT (State Entities)) a)
  deriving (Functor, Applicative, Monad, MonadState Entities,
            MonadError ZWERGError, MonadRandom)

emptyEntities :: Entities
emptyEntities = Entities I.empty I.empty I.empty I.empty
                         I.empty I.empty I.empty I.empty
                         I.empty I.empty I.empty I.empty
                         I.empty I.empty I.empty I.empty I.empty
                         0 (-1)

getNextUUID :: System Int
getNextUUID = do
    uuid <- gets nextUUID
    nextUUID =. (+ 1)
    return uuid

hasComp :: Int -> Component a -> System Bool
hasComp uuid comp = liftM (I.member uuid) (gets comp)

hasNoComp :: Int -> Component a -> System Bool
hasNoComp uuid comp = liftM (I.notMember uuid) (gets comp)

lookupComp :: Int -> Component a -> System (Maybe a)
lookupComp uuid comp = liftM (I.lookup uuid) (gets comp)

unsafeLookupComp :: Int -> Component a -> System a
unsafeLookupComp uuid comp = liftM (fromJust . I.lookup uuid) (gets comp)

filterComp :: (a -> Bool) -> Component a -> System (IntMap a)
filterComp f comp = liftM (I.filter f) (gets comp)

filterCompAtPos :: Position -> Component a -> System (IntMap a)
filterCompAtPos pos component = do
    posMap <- filterComp (== pos) position
    compMap <- gets component
    return $ I.intersection compMap posMap

destroyEntity :: Int -> System ()
destroyEntity uuid = do
    name       =. I.delete uuid
    entityType =. I.delete uuid
    position   =. I.delete uuid
    hp         =. I.delete uuid
    move       =. I.delete uuid
    tileType   =. I.delete uuid
    glyph      =. I.delete uuid
    items      =. I.delete uuid

--TODO: check newtile isn't occupied
makeMove :: Int -> Direction -> System ()
makeMove uuid dir = do

    hasNoPos <- hasNoComp uuid position
    when hasNoPos $ throwError $
      FatalError __LINE__ "Entity.makeMove: tried to move an entity without a position component"

    hasNoLayer <- hasNoComp uuid layer
    when hasNoLayer $ throwError $
      FatalError __LINE__ "Entity.makeMove: tried to move an entity without a layer component"

    oldPos <- unsafeLookupComp uuid position
    let newPos = adjustPosition dir 1 oldPos

    entityLayer <- unsafeLookupComp uuid layer

    oldTileUUID <- tileUUIDatPos entityLayer oldPos
    newTileUUID <- tileUUIDatPos entityLayer newPos

    when (isNothing oldTileUUID) $ throwError $
      FatalError __LINE__ "Entity.makeMove: entity attempted to move away from non-existent tile"
    when (isNothing newTileUUID) $ throwError $
      FatalError __LINE__ "Entity.makeMove: entity attempted to move to a non-existent tile"

    let newTileUUID' = fromJust newTileUUID
        oldTileUUID' = fromJust oldTileUUID

    newTileBlocked <- unsafeLookupComp newTileUUID' blocked

    if newTileBlocked
       then throwError $ PlayerError "You can't move into a wall!"
       else do
            position =. I.insert uuid newPos

            occupants =. I.adjust (filter (/= uuid)) oldTileUUID'
            occupants =. I.adjust (uuid :) newTileUUID'

            needsRedraw =. I.insert oldTileUUID' True
            needsRedraw =. I.insert newTileUUID' True

movePlayer :: Direction -> System ()
movePlayer dir = gets playerUUID >>=  (`makeMove` dir)

adjustPosition ::  Direction -> Int -> Position -> Position
adjustPosition dir amount (x,y) =
    case dir of
      West  -> (x - amount, y)
      East  -> (x + amount, y)
      North -> (x, y - amount)
      South -> (x, y + amount)

isAtPos :: Position -> Int -> System Bool
isAtPos pos uuid = do
    entityPos <- lookupComp uuid position
    when (isNothing entityPos) $ throwError $
      FatalError __LINE__ "Entity.isAtPos: entity has no position"
    return (pos == fromJust entityPos)

tileUUIDatPos :: Int -> Position -> System (Maybe Int)
tileUUIDatPos layerUUID pos = do
    layerTiles <- lookupComp layerUUID tiles
    when (isNothing layerTiles) $ throwError $
      FatalError __LINE__ "Entity.tileUUIDatPos: layerUUID has no assigned tiles"

    tilesAtPos <- filterM (isAtPos pos) $ fromJust layerTiles
    case length tilesAtPos of
      0 -> return Nothing
      1 -> return (Just $ head tilesAtPos)
      _ -> throwError $ FatalError __LINE__
                "Entity.tileUUIDatPos: two or more tiles assigned to the same position"

runSystem :: System t -> PureMT -> Entities -> (Either ZWERGError t, PureMT, Entities)
runSystem (System a) g ets =
    let ((result, ranGen), newEntities) = runState (runRandT (runExceptT a) g) ets
     in (result, ranGen, newEntities)

notEntityType :: Int -> EntityType -> System Bool
notEntityType uuid et = liftM (/= et) $ unsafeLookupComp uuid entityType


tileToGlyph :: Int -> System (Position, Glyph)
tileToGlyph tileUUID = do
    notTile <- notEntityType tileUUID Tile
    when notTile $ throwError $ FatalError __LINE__
        "Entity.tileToGlyph: uuid passed does not have entityType Tile"

    tileOccupants <- unsafeLookupComp tileUUID occupants

    if null tileOccupants
       then do
           tilePos <- unsafeLookupComp tileUUID position
           tileGlyph <- unsafeLookupComp tileUUID glyph
           return (tilePos, tileGlyph)
       else do
           dominantUUID <- getDominantEntity tileOccupants
           domPos <- unsafeLookupComp dominantUUID position
           domGlyph <- unsafeLookupComp dominantUUID glyph
           return (domPos, domGlyph)

getDominantEntity :: [Int] -> System Int
getDominantEntity uuids = do
    etypes <- mapM (`unsafeLookupComp` entityType) uuids
    return $ fst $ maximumBy (comparing snd) $ zip uuids etypes

getUpdatedGlyphs :: Int -> System [(Position, Glyph)]
getUpdatedGlyphs layerUUID = do
    notLayer <- notEntityType layerUUID Layer
    when notLayer $ throwError $ FatalError __LINE__
        "Entity.getGlyphMap: uuid passed does not have entityType Layer"

    layerTiles <- unsafeLookupComp layerUUID tiles
    tilesToRedraw <- filterM (`unsafeLookupComp` needsRedraw) layerTiles
    mapM_ (\tileUUID -> needsRedraw =. I.insert tileUUID False) tilesToRedraw
    mapM tileToGlyph tilesToRedraw

-- printEntity :: Int -> Entities -> IO ()
-- printEntity uuid ets = return ()

--findwithdefault instead of unsafelookup?

-- don't use fmap ( or <$>? )
