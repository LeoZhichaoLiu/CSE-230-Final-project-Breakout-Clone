module Main (main) where

import UI
import Handler
import Game

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)

import Brick
  ( App(..), BrickEvent(..)
    , customMain, halt, neverShowCursor
  )

import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)


-- Define the main application function
appMain :: App GameState Event Name
appMain = App { appDraw = drawApp   -- Draw the application according to gameState
          , appChooseCursor = neverShowCursor  -- default
          , appHandleEvent = eventHandler  -- Handler to your actions (keys)
          , appStartEvent = return ()    -- default
          , appAttrMap = const theMap   -- Definition of attribute
          }

main = do
  -- Create a Brick event channel
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Event
    threadDelay 100000 
  
  -- Initilize the game state, and start the Brick application
  state <- initState
  let buildVty =  mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) appMain state

