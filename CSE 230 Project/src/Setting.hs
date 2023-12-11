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
import Data.List
import qualified Brick.Widgets.Border.Style as BS

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




-- Function to define the event handler
eventHandler :: BrickEvent Name Event -> EventM Name GameState()
eventHandler (AppEvent Event) = do
                                  ifEnd <- use end
                                  ifWin <- use win

                                  if (ifEnd || ifWin) 
                                    then return ()
                                  else do
                                    bullets <- use bullet
                                    bosses <- use boss
                                    enemyAfterBullet <- use enemies
                                    let newBullets = moveBullets bullets 
                                    let newEnemyAfterBullet = bulletKillEnemy bullets enemyAfterBullet
                                    enemies .= newEnemyAfterBullet
                                    bullet .= newBullets
                                    bactPosition <- use bact
                                    let palyerHitByBullet = bulletKillPlayer bactPosition bullets
                                    let newBosses = moveBoss bactPosition bosses
                                    boss .= newBosses
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
                                          let bossBullet = bossShoot bosses
                                          bullet %= (++bossBullet)
                                          -- randomX <- liftIO $ randomRIO (-1, 1)
                                          -- randomY <- liftIO $ randomRIO (-1, 1)
                                          -- food %= over _x (\x -> (x + randomX) `mod` width)
                                          -- food %= over _y (\x -> (x + randomY) `mod` height)
                                        

                                    cur_level <- use level
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
                                    when (catch_by_enemy || palyerHitByBullet) $ do
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
                                                  life .= 1
                                                  win .= False
                                                  end .= False
                                                  score .= 0
                                                  enemies_list <- liftIO $ initEnemy (10)
                                                  enemies .= enemies_list
                                                  level .= 1

eventHandler (VtyEvent (V.EvKey (V.KChar 'g') [])) = do 
                                                  bact .= (V2 10 10)
                                                  win .= False
                                                  end .= False
                                                  score .= 0   
                                                  cur_level <- use level
                                                  level .= (cur_level + 1) 
                                                  enemies_list <- liftIO $ initEnemy (10 + cur_level * 3)
                                                  enemies .= enemies_list

eventHandler (VtyEvent (V.EvKey (V.KChar 'b') [])) = do 
                                                  userPos <- use bact
                                                  oldBulletList <- use bullet
                                                  let bulletList = generateBullet1 True userPos
                                                  bullet .= bulletList ++ oldBulletList
-- Put Esc to quit the game
eventHandler (VtyEvent (V.EvKey V.KEsc []))        = halt
eventHandler _                                     = return ()



theMap :: AttrMap
theMap = attrMap V.defAttr
    [   (attrName "bactAttr", V.black `on` V.black),  
        (attrName "glucAttr", V.black `on` V.black),  
        (attrName "spaceAttr", V.black `on` V.black),  -- Board is White
        (attrName "enemyAttr", V.black `on` V.black),
        (attrName "corpAttr", V.black `on` V.black)
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

isCorpAtPos :: Pos -> [Enemy] -> ([Enemy], Bool)
isCorpAtPos pos enemies = 
  let (corps, remainingEnemies) = partition (\enemy -> _enPos enemy == pos && not (_enAlive enemy) && (_corpsTime enemy)>=0) enemies
      hit = not (null corps)
      update_corps = map (\enemy -> enemy { _corpsTime = 0 }) corps
  in (update_corps++remainingEnemies, hit)

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

updateEnemiesLife :: [Enemy] -> IO [Enemy]
updateEnemiesLife enemies = 
    mapM (\enemy -> updateEnemyLife enemy) enemies


updateEnemyLife :: Enemy -> IO Enemy
updateEnemyLife e = 
  case e of
    Enemy _ _ False (-1) -> do 
                            return e
    Enemy _ _ False 0 -> do 
                        x <- randomRIO (1, width) :: IO Int
                        y <- randomRIO (1, 2) :: IO Int
                        let result = if y == 1 then height else 0
                        return (Enemy (V2 x y) 0 False (-1))
    Enemy p l False n -> return (Enemy p l False (n-1))
    Enemy p 0 True _ -> do 
                        return (Enemy p 0 False corpTime)
    Enemy p n True _ -> return (Enemy p (n-1) True 0)

reviveEnemy :: [Enemy] -> [Enemy]
reviveEnemy [] = []
reviveEnemy (x:xs) = case x of
  Enemy p _ False (-1) -> (Enemy p enemLife True 0):xs
  Enemy _ _ True _ -> x:(reviveEnemy xs)
  Enemy _ _ False _ -> x:(reviveEnemy xs)

-- updateBulletPosition :: [Bullet] -> [Bullet]

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

-- 将处于Pos上的的敌人的 _enAlive 设置为 False
setEnemyDead :: Pos -> [Enemy] -> [Enemy]
setEnemyDead pos enemies = map (\enemy -> if _enPos enemy == pos then enemy { _enAlive = False , _corpsTime = corpTime} else enemy) enemies

-- 判断敌人是否碰撞在Pos这个位置上
isEnemyHitBact :: Pos -> [Enemy] -> ([Enemy], Bool)
isEnemyHitBact pos enemies =
  let (hitEnemies, remainingEnemies) = partition (\enemy -> _enPos enemy == pos && _enAlive enemy) enemies
      hit = not (null hitEnemies)
      updatedEnemies = map (\enemy -> if _enPos enemy == pos then enemy { _enAlive = False, _corpsTime = corpTime } else enemy) hitEnemies
  in (updatedEnemies++remainingEnemies, hit)

addEnemy :: Int -> [Enemy] -> IO [Enemy]
addEnemy 0 cur= do
  return cur
addEnemy n cur= do
  x <- randomRIO (1, width) :: IO Int
  let y = if (n `mod` 2) == 0 then 0 else height
  let enem = Enemy (V2 x y) enemLife True 0
  enemies_list <- liftIO $ addEnemy (n-1) (enem:cur)
  return enemies_list

moveBullets :: [Bullet] -> [Bullet]
moveBullets [] = []
moveBullets (b:bs) = 
  let 
    dir = _bulletDir b
    V2 x y = _bulletPos b
    newPos = case dir of
      N -> V2 x (y+1)
      S -> V2 x (y-1)
      E -> V2 (x+1) y   
      W -> V2 (x-1) y
      NE -> V2 (x+1) (y+1)  
      NW -> V2 (x-1) (y+1)
      SE -> V2 (x+1) (y-1)
      SW -> V2 (x-1) (y-1)
    V2 newX newY = newPos
    in
      if newX<0 || newX>width || newY<0 || newY>height
        then moveBullets bs
        else (b { _bulletPos = newPos}):(moveBullets bs)

generateBullet1 :: Bool -> Pos -> [Bullet]
generateBullet1 f p = [(Bullet p N f),(Bullet p E f),(Bullet p W f),(Bullet p S f)]

generateBullet2 :: Bool -> Pos -> [Bullet]
generateBullet2 f p = [(Bullet p NE f),(Bullet p SE f),(Bullet p NW f),(Bullet p SW f)]

bossShoot :: [Boss] -> [Bullet]
bossShoot [] = []
bossShoot (b:bs) = 
                  let 
                    theForm = _bossForm b
                    bossPos = _bossPos b
                    sleepTime = _bossSleep b
                    ifGenerate = sleepTime == 0
                  in
                    if not ifGenerate
                      then bossShoot bs
                      else 
                        if theForm
                          then generateBullet1 False bossPos ++ bossShoot bs
                          else generateBullet2 False bossPos ++ bossShoot bs


isBulletAtPosFriendly :: Pos -> [Bullet] -> Bool
isBulletAtPosFriendly pos bullets = any (\b -> _bulletPos b  == pos && _friendly b) bullets

isBulletAtPoshostile :: Pos -> [Bullet] -> Bool
isBulletAtPoshostile  pos bullets = any (\b -> _bulletPos b  == pos && not (_friendly b)) bullets

bulletKillEnemy :: [Bullet] -> [Enemy] -> [Enemy]
bulletKillEnemy [] e = e
bulletKillEnemy _ [] = []
bulletKillEnemy (b:bs) e = 
  let
    bulletPos = _bulletPos b
    friendly = _friendly b
    newE = if friendly
            then fst (isEnemyHitBact bulletPos e)
            else e
  in
    bulletKillEnemy bs newE

bulletKillPlayer :: Pos -> [Bullet] -> Bool
bulletKillPlayer p [] = False
bulletKillPlayer p (b:bs) = 
  let 
    bulletPos = _bulletPos b
    friendly = _friendly b
    ifHit = if friendly
              then False
              else bulletPos==p
    in ifHit || bulletKillPlayer p bs

moveBoss :: Pos -> [Boss] -> [Boss]
moveBoss pp [] = []
moveBoss pp (b:bs) = 
  let
    Boss (V2 bx by) form b1 b2 bossLife (V2 tx ty) bT = b
  in 
    if bT > 0
      then 
          Boss (V2 bx by) (not form) b1 b2 bossLife pp (bT-1) : moveBoss pp bs
      else
        if bx==tx && by==ty
          then (Boss (V2 bx by) form b1 b2 bossLife (V2 tx ty) bossSleepTime) : (moveBoss pp bs)
          else 
            case (compare bx tx, compare by ty) of
              (GT, GT) -> (Boss (V2 (bx-1) (by-1)) form (moveBossBody1 (V2 (bx-1) (by-1))) (moveBossBody2 (V2 (bx-1) (by-1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (GT, EQ) -> (Boss (V2 (bx-1) by) form (moveBossBody1 (V2 (bx-1) by)) (moveBossBody2 (V2 (bx-1) by)) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (GT, LT) -> (Boss (V2 (bx-1) (by+1)) form (moveBossBody1 (V2 (bx-1) (by+1))) (moveBossBody2 (V2 (bx-1) (by+1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (EQ, GT) -> (Boss (V2 bx (by-1)) form (moveBossBody1 (V2 bx (by-1))) (moveBossBody2 (V2 bx (by-1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (EQ, LT) -> (Boss (V2 bx (by+1)) form (moveBossBody1 (V2 bx (by+1))) (moveBossBody2 (V2 bx (by+1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (LT, GT) -> (Boss (V2 (bx+1) (by-1)) form (moveBossBody1 (V2 (bx+1) (by-1))) (moveBossBody2 (V2 (bx+1) (by-1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (LT, EQ) -> (Boss (V2 (bx+1) by) form (moveBossBody1 (V2 (bx+1) by)) (moveBossBody2 (V2 (bx+1) by)) bossLife (V2 tx ty) bT) : (moveBoss pp bs)
              (LT, LT) -> (Boss (V2 (bx+1) (by+1)) form (moveBossBody1 (V2 (bx+1) (by+1))) (moveBossBody2 (V2 (bx+1) (by+1))) bossLife (V2 tx ty) bT) : (moveBoss pp bs)

moveBossBody1 :: Pos -> [Pos]
moveBossBody1 (V2 x y) = [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

moveBossBody2 :: Pos -> [Pos]
moveBossBody2 (V2 x y) = [V2 (x+1) (y+1), V2 (x-1) (y+1), V2 (x+1) (y-1), V2 (x-1) (y-1)]

isBossCoreAtPos :: Pos -> [Boss] -> Bool
isBossCoreAtPos pos bosses = any (\b -> _bossPos b == pos) bosses

isBossBody1AtPos_form1 :: Pos -> [Boss] -> Bool
isBossBody1AtPos_form1 pos [] = False
isBossBody1AtPos_form1 pos (b:bs) = 
  let 
    body1 = _bossBody1 b
    theForm = _bossForm b
    isAt = any (\p -> p == pos) body1
  in (isAt && theForm) || (isBossBody1AtPos_form1 pos bs)


isBossBody1AtPos_form2 :: Pos -> [Boss] -> Bool
isBossBody1AtPos_form2 pos [] = False
isBossBody1AtPos_form2 pos (b:bs) = 
  let 
    body1 = _bossBody1 b
    theForm = _bossForm b
    isAt = any (\p -> p == pos) body1
  in (isAt && (not theForm)) || (isBossBody1AtPos_form2 pos bs)

isBossBody2AtPos_form1 :: Pos -> [Boss] -> Bool
isBossBody2AtPos_form1 pos [] = False
isBossBody2AtPos_form1 pos (b:bs) = 
  let 
    body2 = _bossBody2 b
    theForm = _bossForm b
    isAt = any (\p -> p == pos) body2
  in (isAt && theForm) || (isBossBody2AtPos_form1 pos bs)


isBossBody2AtPos_form2 :: Pos -> [Boss] -> Bool
isBossBody2AtPos_form2 pos [] = False
isBossBody2AtPos_form2 pos (b:bs) = 
  let 
    body2 = _bossBody2 b
    theForm = _bossForm b
    isAt = any (\p -> p == pos) body2
  in (isAt && (not theForm)) || (isBossBody2AtPos_form2 pos bs)
