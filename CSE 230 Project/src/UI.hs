{-# LANGUAGE OverloadedStrings #-}

module UI where

import Game
import Utils

import Brick
  ( AttrMap, Widget, Padding(..)
  , hLimit, vBox, hBox
  , padTop, padRight, padLeft, padAll
  , withBorderStyle, str, attrMap, withAttr
  , emptyWidget, attrName, on, fg
  , (<+>)
  )

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), (.=), (%=), over, (&), (%~), use, uses)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)
import Linear.V2 (V2(..), _x, _y)

data Event = Event 
type Name = ()

-- Function to draw the UI of the game, composed of Socre part + board part
drawApp :: GameState -> [Widget Name]
drawApp state = [ C.center 
                  $ padRight (Pad 3) (vBox[(drawLife state), (drawScore state), (drawBullet state)]) 
                  <+>  padRight (Pad 3) (vBox[(drawDead state), (drawWin state)]) 
                  <+> drawBoard state  
                  ]

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

-- Pass Part
drawPass :: GameState -> Widget Name
drawPass state = 
  if ((state ^. level) == 3) && (state ^. win) then 
    withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Final Congratulation!")
    $ padAll 2
    $ (str "You Reach 500 Score!\n You Pass All 3 Levels\n More Levels will be designed in future!")
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

-- Bullet Part
drawBullet :: GameState -> Widget Name
drawBullet state = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Bullet Number")
  $ padAll 2
  -- Update the life everytime according to the game state
  $ str $ (show (state ^. playerBulletNum))

-- Board Part 
drawBoard :: GameState -> Widget Name
drawBoard state = withBorderStyle (BS.borderStyleFromChar ' ')
  $ B.borderWithLabel (str $ ("  Level  " ++ show (state ^. level) ++ "  ")) 
  -- Use vBox + hBox to create board with 40x40 cells
  $ vBox [hBox $ createCellList a | a <- [height-1,height-2..0]]
  where 
    createCellList y = [ createCell (V2 x y) | x <- [0..width-1]]
    -- Draw each cell with different color according to its attributes (bact, food, space etc)
    createCell pos = if pos == (state ^. bact) then withAttr (attrName "bactAttr") (str "🦠")
                     else if isBulletAtPoshostile pos (state ^. bullet) then withAttr (attrName "enemyAttr") (str "🔴")
                     else if isEnemyAtPos pos (state ^. enemies) then withAttr (attrName "enemyAttr") (str "👾")
                     else if isBulletAtPosFriendly pos (state ^. bullet) then withAttr (attrName "enemyAttr") (str "🔵")
                     else if isGlucoseAtPos pos (state ^. glucoses) then withAttr (attrName "glucAttr") (str "🍬")
                     else if isBossCoreAtPos pos (state ^. boss) then withAttr (attrName "enemyAttr") (str "👁️ ")
                     else if isBossBody1AtPos_form1 pos (state ^. boss) then withAttr (attrName "enemyAttr") (str "🟥")
                     else if isBossBody1AtPos_form2 pos (state ^. boss) then withAttr (attrName "enemyAttr") (str "🟪")
                     else if isBossBody2AtPos_form1 pos (state ^. boss) then withAttr (attrName "enemyAttr") (str "🟪")
                     else if isBossBody2AtPos_form2 pos (state ^. boss) then withAttr (attrName "enemyAttr") (str "🟥")
                     else if snd (isCorpAtPos pos (state ^. enemies)) then withAttr (attrName "enemyAttr") (str "⚰️")
                     else  withAttr (attrName "spaceAttr") (str "  ")


theMap :: AttrMap
theMap = attrMap V.defAttr
    [   (attrName "bactAttr", V.black `on` V.black),  
        (attrName "glucAttr", V.black `on` V.black),  
        (attrName "spaceAttr", V.black `on` V.black),  -- Board is White
        (attrName "enemyAttr", V.black `on` V.black),
        (attrName "corpAttr", V.black `on` V.black)
    ]
