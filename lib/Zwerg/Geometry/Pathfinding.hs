module Zwerg.Geometry.Pathfinding (Path, findPath) where

import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Data.Position

import Data.Sequence (Seq, (|>), ViewR(..))
import qualified Data.Sequence as S (empty, viewr, null)
import Lens.Micro.Platform (makeLenses, (%=), (.=), use, view)

-------------------------------------------------------------------------------

newtype Path = MkPath (Seq Direction)

emptyPath :: Path
emptyPath = MkPath S.empty

isEmptyPath :: Path -> Bool
isEmptyPath (MkPath s) = S.null s

pushPath :: Direction -> Path -> Path
pushPath dir (MkPath p) = MkPath $ p |> dir

popPath :: Path -> Path
popPath (MkPath p) =
    case S.viewr p of
      EmptyR -> emptyPath
      oldpath :> _ -> MkPath oldpath

-------------------------------------------------------------------------------

data PathFinderConfig = PathFinderConfig
    { _goal    :: Position
    , _blocked :: Position -> Bool
    }
makeLenses ''PathFinderConfig

data PathFinderContext = PathFinderContext
    { _currentPosition :: Position
    , _currentDistance :: Int
    , _currentPath     :: Path
    , _bestPath        :: Path
    , _bestDistance    :: Int
    }
makeLenses ''PathFinderContext

type PathFinder a = ReaderT PathFinderConfig (State PathFinderContext) a

-------------------------------------------------------------------------------

findPath :: GridMap Bool -> Position -> Position -> Maybe Path
findPath bm startPos goalPos =
    evalState (runReaderT (buildPath >> extractPath) initConfig) initContext
  where initConfig = PathFinderConfig goalPos (zAt bm)
        initContext = PathFinderContext startPos 0 emptyPath emptyPath maxBound

getValidMoveDirPairs :: PathFinder [(Position,Direction)]
getValidMoveDirPairs = do
    blockFunc <- view blocked
    let isGoodPos (p,d) =
            case movePosDir d p of
              Just p' ->
                  if not $ blockFunc p' then Just (p',d) else Nothing
              Nothing -> Nothing
    cp <- use currentPosition
    return $ mapMaybe isGoodPos $ zip (repeat cp) cardinalDirections

buildPath :: PathFinder ()
buildPath = do
    cd <- use currentDistance
    md <- use bestDistance
    cp <- use currentPosition
    atGoal <- (cp ==) <$> view goal

    when (atGoal && cd < md) $ do
        bestDistance .= cd
        use currentPath >>= (.=) bestPath

    when (not atGoal && cd < md) $ do
        nextPositions <- getValidMoveDirPairs
        forM_ nextPositions $ \(pos,dir) -> do
            currentPath %= pushPath dir
            --FIXME: account for extra diagonal distance
            currentDistance %= succ
            currentPosition .= pos

            buildPath

            currentPath %= popPath
            currentDistance %= pred
        currentPosition .= cp

{-# INLINABLE extractPath #-}
extractPath :: PathFinder (Maybe Path)
extractPath = do
    bp <- use bestPath
    if not $ isEmptyPath bp
       then return $ Just bp
       else return Nothing
