{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Utils where

import Game

import Control.Monad.IO.Class (liftIO)
import Control.Lens ((^.), (.=), (%=), over, (&), (%~), use, uses)

import Linear.V2 (V2(..), _x, _y)
import System.Random (randomRIO)
import Data.List


step :: GameState -> GameState
step g = g & score %~ (+1)

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

bulletKillBoss :: [Bullet] -> [Boss] -> [Boss]
bulletKillBoss [] b = b
bulletKillBoss _ [] = []
bulletKillBoss (b:bs) e = 
  let
    bulletPos = _bulletPos b
    friendly = _friendly b
    newE = if friendly
            then bulletKillBossHelper bulletPos e
            else e
  in
    bulletKillBoss bs newE

bulletKillBossHelper :: Pos -> [Boss] -> [Boss]
bulletKillBossHelper p [] = []
bulletKillBossHelper p (b:bs) = 
  let 
    bosscore = _bossPos b
    body1 = _bossBody1 b
    body2 = _bossBody2 b
    health = _bossLife b
    ifHit = bosscore==p || any (\x -> x == p) body1 || any (\x -> x == p) body2
  in
    if ifHit 
      then if (health-1)<=0
              then bulletKillBossHelper p bs
              else b {_bossLife = health-1} : bulletKillBossHelper p bs
      else
        b:bulletKillBossHelper p bs

