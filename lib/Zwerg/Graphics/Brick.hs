module Zwerg.Graphics.Brick where

import Zwerg.Prelude hiding ((<>))

import Zwerg
import Zwerg.Event.Queue
import Zwerg.Game
import Zwerg.Graphics.Brick.Builder
import Zwerg.Log
import Zwerg.Random
import Zwerg.UI.Input

import Brick.AttrMap
import Brick.Util (on, fg)
import Brick.Widgets.ProgressBar
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Graphics.Vty as VTY
import Data.Monoid ((<>))

handleEventZwerg :: HasCallStack
                 => ZwergState
                 -> BT.BrickEvent () ZwergEvent
                 -> BT.EventM () (BT.Next ZwergState)
handleEventZwerg zs (BT.VtyEvent ev) =
  let eventVTYtoZwergInput :: VTY.Event -> Maybe KeyCode
      eventVTYtoZwergInput (VTY.EvKey VTY.KEsc []) = Just Escape
      eventVTYtoZwergInput (VTY.EvKey (VTY.KChar ch) []) = Just $ KeyChar ch
      eventVTYtoZwergInput (VTY.EvKey VTY.KEnter []) = Just Return
      eventVTYtoZwergInput _ = Nothing
  in case eventVTYtoZwergInput ev of
       Just Escape -> BM.halt zs
       Just key -> do
         let st = view gameState zs
             st' = runGame (processUserInput key) (view ranGen zs) st
             zs' = set gameState st' zs
             badPlayerInput = view (gameState . playerGoofed) zs'
         if badPlayerInput
           then BM.continue
                $ set (gameState . eventQueue) zDefault
                $ set (gameState . userLog) (view (gameState . userLog) zs')
                $ zs
           else BM.continue zs'
       _ -> BM.continue zs

handleEventZwerg a b = BM.resizeOrQuit a b

theMap :: AttrMap
theMap = attrMap VTY.defAttr
    [ ("inventory" <> "focused"        , VTY.black `Brick.Util.on` VTY.white )
    , ("inventory" <> "unfocused"      , VTY.white `Brick.Util.on` VTY.black )
    , ("inventory" <> "drop_focused"   , VTY.black `Brick.Util.on` VTY.red   )
    , ("inventory" <> "drop_unfocused" , VTY.white `Brick.Util.on` VTY.red   )
    , ("logo" , fg VTY.red)
    , (progressCompleteAttr            , VTY.black `on` VTY.green            )
    , (progressIncompleteAttr          , VTY.black `on` VTY.red              )
    ]

zwergApp :: BM.App ZwergState ZwergEvent ()
zwergApp = BM.App
  { BM.appDraw         = buildZwergUI
  , BM.appHandleEvent  = handleEventZwerg
  , BM.appStartEvent   = return
  , BM.appAttrMap      = const theMap
  , BM.appChooseCursor = BM.neverShowCursor
  }

initBrick :: (HasCallStack, MonadIO m) => m ()
initBrick = do
  gen <- newPureRanGen
  void $ liftIO $ BM.defaultMain zwergApp $ set ranGen gen initZwergState
