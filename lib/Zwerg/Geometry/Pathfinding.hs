module Zwerg.Geometry.Pathfinding (Path, findPath) where

import Zwerg.Prelude
import Zwerg.Data.GridMap
import Zwerg.Data.Position

import Data.Sequence (Seq, (|>), ViewR(..))
import qualified Data.Sequence as S (empty, viewr, null)

-------------------------------------------------------------------------------

newtype Path = MkPath (Seq Direction)

emptyPath :: Path
emptyPath = MkPath S.empty

isEmptyPath :: Path -> Bool
isEmptyPath (MkPath s) = S.null s

-------------------------------------------------------------------------------

type PathFinder a = ReaderT PathFinderConfig (State PathFinderContext) a

data PathFinderConfig = PathFinderConfig
    { goal    :: Position
    , blocked :: Position -> Bool
    }

data PathFinderContext = PathFinderContext
    { currentPosition :: Position
    , currentDistance :: Int
    , currentPath     :: Path
    , bestPath        :: Path
    , bestDistance    :: Int
    }

-------------------------------------------------------------------------------

findPath :: GridMap Bool -> Position -> Position -> Maybe Path
findPath blocked startPos goalPos = evalState (runReaderT (buildPath >> extractPath) initConfig) initContext
    where initConfig = PathFinderConfig goalPos (zAt blocked)
          initContext = PathFinderContext startPos 0 emptyPath emptyPath maxBound

modCurrentDistance :: (Int -> Int) -> PathFinder ()
modCurrentDistance f = do
    cd <- gets currentDistance
    get >>= \s -> put $ s { currentDistance = f cd }

setBestDistance :: Int -> PathFinder ()
setBestDistance i = get >>= \s -> put $ s { bestDistance = i }

setBestPath :: Path -> PathFinder ()
setBestPath p = get >>= \s -> put $ s { bestPath = p }

setCurrentPosition :: Position -> PathFinder ()
setCurrentPosition p = get >>= \s -> put $ s { currentPosition = p }

pushPath :: Direction -> PathFinder ()
pushPath dir = do
    (MkPath cp) <- gets currentPath
    get >>= \s -> put $ s { currentPath = MkPath $ cp |> dir }

popPath :: PathFinder ()
popPath = do
    (MkPath cp) <- gets currentPath
    case (S.viewr cp) of
      EmptyR -> return ()
      (oldpath :> _) -> get >>= \s -> put $ s { currentPath = MkPath $ oldpath }

getValidMoves :: PathFinder [(Position,Direction)]
getValidMoves = do
    blockFunc <- asks blocked
    let isGoodPos (p,d) =
            case (movePosDir d p) of
              Just p' -> if (not $ blockFunc p')
                            then Just (p',d)
                            else Nothing
              Nothing -> Nothing

    cp <- gets currentPosition
    return $ mapMaybe isGoodPos $ zip (repeat cp) allDirections

buildPath :: PathFinder ()
buildPath = do
    cd <- gets currentDistance
    md <- gets bestDistance
    cp <- gets currentPosition
    atGoal <- (cp ==) <$> asks goal

    when (atGoal && cd < md) $ do
        setBestDistance cd
        gets currentPath >>= setBestPath

    when (not atGoal && cd < md) $ do
        nextPositions <- getValidMoves
        forM_ nextPositions $ \(pos,dir) -> do
            pushPath dir
            --FIXME: account for extra diagonal distance
            modCurrentDistance succ
            setCurrentPosition pos

            buildPath

            popPath
            modCurrentDistance pred
        setCurrentPosition cp

extractPath :: PathFinder (Maybe Path)
extractPath = do
    bp <- gets bestPath
    if (not $ isEmptyPath bp)
       then return $ Just bp
       else return Nothing
