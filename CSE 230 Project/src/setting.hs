{-# LANGUAGE OverloadedStrings #-}
module Setting where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)
import Game

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Widget
  , customMain, neverShowCursor
  , halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, attrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), (.=))
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))


data Event = Event 

type Name = () -- Useless

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

  -- Start the Brick application
  state <- initState
  -- Start the Brick application
  let buildVty =  mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) appMain state


-- Function to draw the UI of the game, composed of Socre part + board part
drawApp :: GameState -> [Widget Name]
drawApp state = 
  [ C.center $ padRight (Pad 3) (drawScore state) <+> drawBoard state ]

-- Score Part
drawScore :: GameState -> Widget Name
drawScore state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ padAll 2
  -- Update the score everytime according to the game state
  $ str $ (show (state ^. dir))

-- Board Part 
drawBoard :: GameState -> Widget Name
drawBoard state = withBorderStyle BS.unicodeBold
  -- Use vBox + hBox to create board with 20x20 cells
  $ vBox [hBox $ createCellList a | a <- [height-1,height-2..0]]
  where 
    createCellList y = [ createCell (V2 x y) | x <- [0..width-1]]
    -- Draw each cell with different color according to its attributes (bact, food, space etc)
    createCell pos = if pos == (state ^. bact) then withAttr (attrName "bactAttr") (str "  ")
                     else if pos == (state ^. food) then withAttr (attrName "foodAttr") (str "  ")
                     else  withAttr (attrName "spaceAttr") (str "  ")

--cw :: Widget Name
--cw = str "  "


-- Function to define the event handler
eventHandler :: BrickEvent Name Event -> EventM Name GameState()
eventHandler (VtyEvent (V.EvKey V.KRight []))          = do 
                                                        dir .= East
eventHandler (VtyEvent (V.EvKey V.KLeft []))         = do 
                                                        dir .= West
eventHandler (VtyEvent (V.EvKey V.KUp []))         = do
                                                     dir .= North
eventHandler (VtyEvent (V.EvKey V.KDown []))         = do
                                                     dir .= South
-- Put Esc to quit the game
eventHandler (VtyEvent (V.EvKey V.KEsc []))        = halt
eventHandler _                                     = return ()


-- Define the Attribute Map
theMap :: AttrMap
theMap = attrMap V.defAttr
    [   (attrName "bactAttr", V.red `on` V.red),  -- Bacterial is Red
        (attrName "foodAttr", V.green `on` V.green),  -- Food is Green
        (attrName "spaceAttr", V.white `on` V.white)  -- Board is White
    ]
