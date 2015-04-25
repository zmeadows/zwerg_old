{-# LANGUAGE OverloadedStrings #-}

import Zwerg.Core
import Zwerg.Entity
import Zwerg.Layer.Simple
import Zwerg.Port
import Zwerg.Entity.Player
import Zwerg.Types

import Control.Monad.State.Strict hiding (get, gets)
import Data.Label
import System.Random.Mersenne.Pure64

import Control.Monad.IO.Class (liftIO)

import UI.NCurses as NC

import Data.Map.Strict (Map)
import Data.Map.Strict as M (toList)

initTestEntities :: System ()
initTestEntities = do
    layerUUID <- makeSimpleLayer 15 15
    void $ makePlayer "Bob" (5,5) 10 layerUUID


main :: IO ()
main = do
    pt <- newPureMT
    let zsInit = initZwergState emptyEntities pt
        zs = execState (modifyEntities $ initTestEntities >> return OverWorld) zsInit
    runCurses $ do
        setEcho False
        _ <- setCursorMode CursorInvisible
        w <- defaultWindow
        drawScreen w (get glyphMap zs)
        render
        updateLoop w zs
        return ()

updateLoop :: Window -> ZWERGState -> Curses ()
updateLoop w zs = do
    ev <- getEvent w Nothing
    case ev of
      Just (EventCharacter 'q') -> return ()
      Just (EventCharacter ch) -> do
        let f = modifyEntities $ processInput (get port zs) (None, Letter ch)
            zs' = execState f zs
        -- liftIO $ print (get glyphMap zs')
        drawScreen w (get glyphMap zs')
        render
        updateLoop w zs'
      _ -> updateLoop w zs

drawScreen :: Window -> Map Position Zwerg.Types.Glyph -> Curses ()
drawScreen w gm = mapM_ (drawGlyph w) $ M.toList gm


drawGlyph :: Window -> (Position, Zwerg.Types.Glyph) -> Curses ()
drawGlyph w ((x,y), (ch, _, _)) =
    updateWindow w $ do
         moveCursor (fromIntegral y ) (fromIntegral x )
         drawString [ch]

