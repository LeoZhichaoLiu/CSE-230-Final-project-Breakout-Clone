{-# LANGUAGE OverloadedStrings #-}
module Setting where

import Control.Monad (forever, void, when)
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
import Control.Lens ((^.), (.=), (%=), over, (&), (%~), use, uses)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (randomRIO)

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
drawApp state = [ C.center $ padRight (Pad 3) (vBox[(drawLife state), (drawScore state)]) 
                  <+> drawBoard state <+>  padRight (Pad 3) (vBox[(drawDead state), (drawWin state)]) ]


-- Dead Part
drawDead :: GameState -> Widget Name
drawDead state = 
  if (state ^. end) then 
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Unfortunately")
    $ padAll 3
    $ str "You Are Dead!\nClick R to Restart"
  else emptyWidget

-- Win Part
drawWin :: GameState -> Widget Name
drawWin state = 
  if (state ^. win) then 
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Congratulation!")
    $ padAll 2
    $ (str "You Reach 500 Score!\nClick G to Next Level")
  else emptyWidget

-- Score Part
drawScore :: GameState -> Widget Name
drawScore state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ padAll 2
  -- Update the score everytime according to the game state
  $ str $ (show (state ^. score))

-- Life Part
drawLife :: GameState -> Widget Name
drawLife state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Life")
  $ padAll 2
  -- Update the life everytime according to the game state
  $ str $ (show (state ^. life))


-- Board Part 
drawBoard :: GameState -> Widget Name
drawBoard state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str $ ("  Level  " ++ show (state ^. level) ++ "  ")) 
  -- Use vBox + hBox to create board with 40x40 cells
  $ vBox [hBox $ createCellList a | a <- [height-1,height-2..0]]
    where 
    createCellList y = [ createCell (V2 x y) | x <- [0..width-1]]
    -- Draw each cell with different color according to its attributes (bact, food, space etc)
    createCell pos = if pos == (state ^. bact) then withAttr (attrName "bactAttr") (str "🦠")
                     else if isGlucoseAtPos pos (state ^. glucoses) then withAttr (attrName "glucAttr") (str "🍬")
                     else if isEnemyAtPos pos (state ^. enemies) then withAttr (attrName "enemyAttr") (str "👾")
                     else  withAttr (attrName "spaceAttr") (str "  ")




-- Function to define the event handler
eventHandler :: BrickEvent Name Event -> EventM Name GameState()
eventHandler (AppEvent Event) = do
                                  ifEnd <- use end
                                  ifWin <- use win

                                  if (ifEnd || ifWin) 
                                    then return ()
                                  else do
                                    bactPosition <- use bact
                                    --foodPosition <- use food
                                    --randomX <- liftIO $ randomRIO (-1, 1)
                                    --randomY <- liftIO $ randomRIO (-1, 1)
                                    -- --
                                    -- food %= over _x (\x -> (x + randomX) `mod` width)
                                    -- food %= over _y (\x -> (x + randomY) `mod` height)
                                    the_score <- use score
                                    if (the_score `mod` 2 == 0)
                                        then do
                                          oldEnemies <- use enemies
                                          newEnemies <- liftIO $ updateEnemyPos bactPosition oldEnemies
                                          let newEnemies' = updateEnemiesLife newEnemies
                                          enemies .= newEnemies'

                                          oldGlucoses <- use glucoses
                                          newGlucoses <- liftIO $ updateGlucosePos bactPosition oldGlucoses
                                          glucoses .= newGlucoses

                                    else do
                                          -- randomX <- liftIO $ randomRIO (-1, 1)
                                          -- randomY <- liftIO $ randomRIO (-1, 1)
                                          -- food %= over _x (\x -> (x + randomX) `mod` width)
                                          -- food %= over _y (\x -> (x + randomY) `mod` height)
                                          bact .= bactPosition

                                    cur_level <- use level
                                    when (cur_level == 2 && the_score > 0 && the_score `mod` 100 == 0) $ do
                                      let num = the_score `div` 100
                                      enemies_list <- liftIO $ initEnemy (10 + num * 3)
                                      enemies .= enemies_list

                                    cur_glucoses <- use glucoses
                                    when (isGlucoseAtPos bactPosition cur_glucoses) $ do
                                        cur_life <- use life
                                        life .= (cur_life + 1)
                                        (cur_g :| next_g) <- use future_glucoses
                                        future_glucoses .= next_g
                                        glucoses .= [Glucose (cur_g)]

                                    enemies' <- use enemies
                                    cur_life <- use life
                                    let catch_by_enemy = (isEnemyAtPos bactPosition enemies') 
                                    when (catch_by_enemy) $ do
                                      life .= (cur_life - 1)

                                    cur_life <- use life
                                    let dead = (cur_life <= 0)

                                    if dead then do
                                      end .= True
                                    else if (the_score == 500) then do
                                      win .= True
                                    else do 
                                      score %= (+1) 
                                   

eventHandler (VtyEvent (V.EvKey V.KRight [])) = do 
                                                  ifEnd <- use end
                                                  ifWin <- use win
                                                  if (ifEnd || ifWin) 
                                                    then return ()
                                                  else do
                                                    dir .= East
                                                    bact %= over _x (\x -> (x + 1) `mod` width)
eventHandler (VtyEvent (V.EvKey V.KLeft [])) = do      
                                                  ifEnd <- use end
                                                  ifWin <- use win
                                                  if (ifEnd || ifWin) 
                                                    then return ()
                                                  else do
                                                    dir .= West   
                                                    bact %= over _x (\x -> (x - 1) `mod` width)
eventHandler (VtyEvent (V.EvKey V.KUp [])) = do
                                                ifEnd <- use end
                                                ifWin <- use win
                                                if (ifEnd || ifWin) 
                                                  then return ()
                                                else do
                                                  dir .= North
                                                  bact %= over _y (\x -> (x + 1) `mod` height)
eventHandler (VtyEvent (V.EvKey V.KDown [])) = do
                                                  ifEnd <- use end
                                                  ifWin <- use win
                                                  if (ifEnd || ifWin) 
                                                    then return ()
                                                  else do
                                                    dir .= South
                                                    bact %= over _y (\x -> (x - 1) `mod` height)

eventHandler (VtyEvent (V.EvKey (V.KChar 'r') [])) = do 
                                                  bact .= (V2 10 10)
                                                  life .= 1
                                                  win .= False
                                                  end .= False
                                                  score .= 0
eventHandler (VtyEvent (V.EvKey (V.KChar 'g') [])) = do 
                                                  bact .= (V2 10 10)
                                                  win .= False
                                                  end .= False
                                                  score .= 0   
                                                  cur_level <- use level
                                                  level .= (cur_level + 1)               
-- Put Esc to quit the game
eventHandler (VtyEvent (V.EvKey V.KEsc []))        = halt
eventHandler _                                     = return ()


-- Define the Attribute Map

theMap :: AttrMap
theMap = attrMap V.defAttr
    [   (attrName "bactAttr", V.white `on` V.white),  -- Bacterial is Red
        (attrName "glucAttr", V.white `on` V.white),  -- Glucose is Green
        (attrName "spaceAttr", V.white `on` V.white),  -- Board is White
        (attrName "enemyAttr", V.white `on` V.white) -- Enemy is black
    ]


-- Original
-- chase :: Pos -> Pos -> Pos
-- chase foodPos bactPos =
--   case compare (foodPos ^._x) (bactPos ^._x) of
--     LT -> foodPos & _x %~ (\x -> (x + 1) `mod` width)
--     GT -> foodPos & _x %~ (\x -> (x - 1) `mod` width)
--     EQ -> case compare (foodPos ^._y) (bactPos ^._y) of
--       LT -> foodPos & _y %~ (\y -> (y + 1) `mod` height)
--       GT -> foodPos & _y %~ (\y -> (y - 1) `mod` height)
--       EQ -> foodPos -- Food has reached the bacteria


chaseX :: Pos -> Pos -> Pos
chaseX thePos bactPos =
  case compare (thePos ^._x) (bactPos ^._x) of
    LT -> thePos & _x %~ (\x -> (x + 1) `mod` width)
    GT -> thePos & _x %~ (\x -> (x - 1) `mod` width)
    EQ -> thePos -- Food has reached the bacteria

chaseY :: Pos -> Pos -> Pos
chaseY thePos bactPos =
  case compare (thePos ^._y) (bactPos ^._y) of
    LT -> thePos & _y %~ (\x -> (x + 1) `mod` height)
    GT -> thePos & _y %~ (\x -> (x - 1) `mod` height)
    EQ -> thePos -- Food has reached the bacteria

isEnemyAtPos :: Pos -> [Enemy] -> Bool
isEnemyAtPos pos enemies = any (\enemy -> _enPos enemy == pos && _enAlive enemy) enemies

updateEnemyPos :: Pos -> [Enemy] -> IO [Enemy]
updateEnemyPos bacPos enemies = 
    mapM (\enemy -> updateEnemyChasePlayer bacPos enemy) enemies

updateEnemyChasePlayer :: Pos -> Enemy -> IO Enemy
updateEnemyChasePlayer playerPos enemy =
  if _enAlive enemy
    then do
      ifX <- randomRIO (1, 2) :: IO Int
      if ifX == 1
        then do
          let newPos = chaseX (_enPos enemy) playerPos
          return $ enemy {_enPos = newPos}
        else do
          let newPos = chaseY (_enPos enemy) playerPos
          return $ enemy {_enPos = newPos}
    else return enemy

updateEnemiesLife :: [Enemy] -> [Enemy]
updateEnemiesLife enemies = 
    map (\enemy -> updateEnemyLife enemy) enemies


updateEnemyLife :: Enemy -> Enemy
updateEnemyLife e = case e of
  Enemy _ _ False -> e
  Enemy p 0 True -> Enemy p 0 False
  Enemy p n True -> Enemy p (n-1) True 


isGlucoseAtPos :: Pos -> [Glucose] -> Bool
isGlucoseAtPos pos glucoses = any (\g -> _gluPos g == pos) glucoses

updateGlucosePos :: Pos -> [Glucose] -> IO [Glucose]
updateGlucosePos bacPos glucoses = 
    mapM (\g -> updateGlucoseAntiChasePlayer bacPos g) glucoses

updateGlucoseAntiChasePlayer :: Pos -> Glucose -> IO Glucose
updateGlucoseAntiChasePlayer playerPos g = do
    ifX <- randomRIO (1, 100) :: IO Int
    if ifX == 1
      then do
        let newPos = antiChaseX (_gluPos g) playerPos
        return $ g {_gluPos = newPos}
      else do
        let newPos = antiChaseY (_gluPos g) playerPos
        return $ g {_gluPos = newPos}

antiChaseX :: Pos -> Pos -> Pos
antiChaseX thePos bactPos =
  case compare (thePos ^._x) (bactPos ^._x) of
    LT -> thePos & _x %~ (\x -> (x + 1) `mod` width)
    GT -> thePos & _x %~ (\x -> (x - 1) `mod` width)
    EQ -> thePos -- Glucose has reached the bacteria

antiChaseY :: Pos -> Pos -> Pos
antiChaseY thePos bactPos =
    thePos & _y %~ (\x -> (x - 1) `mod` height)
