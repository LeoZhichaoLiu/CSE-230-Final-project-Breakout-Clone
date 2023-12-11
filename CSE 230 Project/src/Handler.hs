{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Game
import Utils
import UI (Event(..), Name)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Linear.V2 (V2(..), _x, _y)

import Brick
  ( BrickEvent(..), EventM, halt
  )

import Control.Lens ((^.), (.=), (%=), over, (&), (%~), use, uses)
import qualified Graphics.Vty as V
import Graphics.Vty.Platform.Unix (mkVty)


-- Function to define the event handlers
eventHandler :: BrickEvent Name Event -> EventM Name GameState()
eventHandler (AppEvent Event) = do
                                  ifEnd <- use end
                                  ifWin <- use win

                                  if (ifEnd || ifWin) 
                                    then return ()
                                  else do
                                    bactPosition <- use bact
                                    cur_level <- use level
                                    --when (cur_level >= 3) $ do
                                    bullets <- use bullet
                                    bosses <- use boss
                                    enemyAfterBullet <- use enemies
                                    let newBullets = moveBullets bullets 
                                    let newEnemyAfterBullet = bulletKillEnemy bullets enemyAfterBullet
                                    enemies .= newEnemyAfterBullet
                                    bullet .= newBullets
                                    let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                    let newBosses = moveBoss bactPosition bosses
                                    boss .= newBosses
                                    cur_life <- use life
                                    when (palyerHitByBullet) $ do
                                            life .= (cur_life - 1)
                                    --foodPosition <- use food
                                    --randomX <- liftIO $ randomRIO (-1, 1)
                                    --randomY <- liftIO $ randomRIO (-1, 1)
                                    -- --
                                    -- food %= over _x (\x -> (x + randomX) `mod` width)
                                    -- food %= over _y (\x -> (x + randomY) `mod` height)
                                    the_score <- use score
                                    when (the_score `mod` 10 == 0) $ do
                                            oldEnemies <- use enemies
                                            let newEnemies = reviveEnemy oldEnemies
                                            enemies .= newEnemies
                                    when (the_score `mod` 2 == 0) $ do
                                          oldEnemies <- use enemies
                                          newEnemies <- liftIO $ updateEnemyPos bactPosition oldEnemies
                                          newEnemies' <- liftIO $ updateEnemiesLife newEnemies
                                          enemies .= newEnemies'

                                          oldGlucoses <- use glucoses
                                          newGlucoses <- liftIO $ updateGlucosePos bactPosition oldGlucoses
                                          glucoses .= newGlucoses
                                          --when (cur_level >= 3) $ do
                                          bosses <- use boss
                                          let bossBullet = bossShoot bosses
                                          bullet %= (++bossBullet)
                                          -- randomX <- liftIO $ randomRIO (-1, 1)
                                          -- randomY <- liftIO $ randomRIO (-1, 1)
                                          -- food %= over _x (\x -> (x + randomX) `mod` width)
                                          -- food %= over _y (\x -> (x + randomY) `mod` height)
                                        

                                    when (cur_level > 1 && the_score > 0 && the_score `mod` 100 == 0) $ do
                                      let num = the_score `div` 100
                                      oldEnemies <- use enemies
                                      enemies_list <- liftIO $ addEnemy ((cur_level-1) * 3) oldEnemies -- update the new enemies list
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
                                    let (newEnemyList, catch_by_enemy) = (isEnemyHitBact bactPosition enemies') 
                                    enemies .= newEnemyList
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
                                                    dir .= E
                                                    bact %= over _x (\x ->
                                                        if x + 1 < width
                                                          then (x + 1)
                                                          else x
                                                      )
                                                  bactPosition <- use bact
                                                  enemies' <- use enemies
                                                  bullets <- use bullet
                                                  cur_life <- use life
                                                  let (newEnemyList, catch_by_enemy) = isEnemyHitBact bactPosition enemies' 
                                                  let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                                  enemies .= newEnemyList
                                                  when (catch_by_enemy || palyerHitByBullet) $ do
                                                    life .= (cur_life - 1)
                                                  cur_glucoses <- use glucoses
                                                  when (isGlucoseAtPos bactPosition cur_glucoses) $ do
                                                      cur_life <- use life
                                                      life .= (cur_life + 1)
                                                      (cur_g :| next_g) <- use future_glucoses
                                                      future_glucoses .= next_g
                                                      glucoses .= [Glucose (cur_g)]
                                                  let (newnewEnemyList, corpHit) = isCorpAtPos bactPosition newEnemyList
                                                  when (corpHit) $ do
                                                    playerBulletNum %= (+1)
                                                    enemies .= newnewEnemyList



eventHandler (VtyEvent (V.EvKey V.KLeft [])) = do      
                                                  ifEnd <- use end
                                                  ifWin <- use win
                                                  if (ifEnd || ifWin) 
                                                    then return ()
                                                  else do
                                                    dir .= W   
                                                    bact %= over _x (\x ->
                                                        if x - 1 > -1
                                                          then (x - 1)
                                                          else x)
                                                  bactPosition <- use bact
                                                  enemies' <- use enemies
                                                  bullets <- use bullet
                                                  cur_life <- use life
                                                  let (newEnemyList, catch_by_enemy) = isEnemyHitBact bactPosition enemies' 
                                                  let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                                  enemies .= newEnemyList
                                                  when (catch_by_enemy || palyerHitByBullet) $ do
                                                    life .= (cur_life - 1)
                                                  cur_glucoses <- use glucoses
                                                  when (isGlucoseAtPos bactPosition cur_glucoses) $ do
                                                      cur_life <- use life
                                                      life .= (cur_life + 1)
                                                      (cur_g :| next_g) <- use future_glucoses
                                                      future_glucoses .= next_g
                                                      glucoses .= [Glucose (cur_g)]
                                                  let (newnewEnemyList, corpHit) = isCorpAtPos bactPosition newEnemyList
                                                  when (corpHit) $ do
                                                    playerBulletNum %= (+1)
                                                    enemies .= newnewEnemyList
eventHandler (VtyEvent (V.EvKey V.KUp [])) = do
                                                ifEnd <- use end
                                                ifWin <- use win
                                                if (ifEnd || ifWin) 
                                                  then return ()
                                                else do
                                                  dir .= N
                                                  bact %= over _y (\x ->
                                                        if x + 1 < height
                                                          then (x + 1)
                                                          else x
                                                      )
                                                  bactPosition <- use bact
                                                  enemies' <- use enemies
                                                  bullets <- use bullet
                                                  cur_life <- use life
                                                  let (newEnemyList, catch_by_enemy) = isEnemyHitBact bactPosition enemies' 
                                                  let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                                  enemies .= newEnemyList
                                                  when (catch_by_enemy || palyerHitByBullet) $ do
                                                    life .= (cur_life - 1)
                                                  cur_glucoses <- use glucoses
                                                  when (isGlucoseAtPos bactPosition cur_glucoses) $ do
                                                      cur_life <- use life
                                                      life .= (cur_life + 1)
                                                      (cur_g :| next_g) <- use future_glucoses
                                                      future_glucoses .= next_g
                                                      glucoses .= [Glucose (cur_g)]
                                                  let (newnewEnemyList, corpHit) = isCorpAtPos bactPosition newEnemyList
                                                  when (corpHit) $ do
                                                    playerBulletNum %= (+1)
                                                    enemies .= newnewEnemyList
eventHandler (VtyEvent (V.EvKey V.KDown [])) = do
                                                  ifEnd <- use end
                                                  ifWin <- use win
                                                  if (ifEnd || ifWin) 
                                                    then return ()
                                                  else do
                                                    dir .= S
                                                    bact %= over _y (\x ->
                                                        if x - 1 > -1
                                                          then (x - 1)
                                                          else x)
                                                  bactPosition <- use bact
                                                  enemies' <- use enemies
                                                  bullets <- use bullet
                                                  cur_life <- use life
                                                  let (newEnemyList, catch_by_enemy) = isEnemyHitBact bactPosition enemies' 
                                                  let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                                  enemies .= newEnemyList
                                                  when (catch_by_enemy || palyerHitByBullet) $ do
                                                    life .= (cur_life - 1)
                                                  cur_glucoses <- use glucoses
                                                  when (isGlucoseAtPos bactPosition cur_glucoses) $ do
                                                      cur_life <- use life
                                                      life .= (cur_life + 1)
                                                      (cur_g :| next_g) <- use future_glucoses
                                                      future_glucoses .= next_g
                                                      glucoses .= [Glucose (cur_g)]
                                                  let (newnewEnemyList, corpHit) = isCorpAtPos bactPosition newEnemyList
                                                  when (corpHit) $ do
                                                    playerBulletNum %= (+1)
                                                    enemies .= newnewEnemyList

eventHandler (VtyEvent (V.EvKey (V.KChar 'r') [])) = do 
                                                  bact .= (V2 10 10)
                                                  win .= False
                                                  end .= False
                                                  score .= 0
                                                  enemies_list <- liftIO $ initEnemy (10)
                                                  enemies .= enemies_list
                                                  cur_life <- use life
                                                  when (cur_life <= 0) $ do
                                                    life .= 1

eventHandler (VtyEvent (V.EvKey (V.KChar 'g') [])) = do 
                                                  cur_level <- use level
                                                  if (cur_level < 3) then do
                                                    bact .= (V2 10 10)
                                                    win .= False
                                                    end .= False
                                                    score .= 0   
                                                    level .= (cur_level + 1) 
                                                    enemies_list <- liftIO $ initEnemy (10 + cur_level * 3)
                                                    enemies .= enemies_list
                                                    when (cur_level == 2) $ do
                                                        boss .= [generateBoss (V2 40 40)]
                                                   else return ()

eventHandler (VtyEvent (V.EvKey (V.KChar 'b') [])) = do 
                                                  cur_level <- use level
                                                  userPos <- use bact
                                                  oldBulletList <- use bullet
                                                  let bulletList = generateBullet1 True userPos
                                                  bullet .= bulletList ++ oldBulletList                                            
-- Put Esc to quit the game
eventHandler (VtyEvent (V.EvKey V.KEsc []))        = halt
eventHandler _                                     = return ()
