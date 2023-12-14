{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), randomRIO, newStdGen)

-- Constant Definition
height, width, enemLife, corpTime, bossLife :: Int
height = 40
width = 50
enemLife = 80
corpTime = 20
bossLife = 10
bossSleepTime = 11

type Pos = V2 Int

-- Data Definition
data Direction
  = N
  | S
  | E
  | W
  | NE
  | NW
  | SE
  | SW
  deriving (Eq, Show)

data List a = a :| List a
  deriving (Show)

data GameState = GameState
  { _bact  :: Pos        -- Bacteria Position
  , _dir    :: Direction    -- Current Bacteria Position (to determine what it should go)
  , _life  :: Int          -- Bacteria Life
  , _playerBulletNum :: Int    -- Bacteria's bullets to kill bos
  , _bullet :: [Bullet]        -- List of Bullets
  , _glucoses   :: [Glucose]       -- Glucose Position
  , _future_glucoses :: List Pos   -- Future possible glucose position
  , _enemies  :: [Enemy]  -- List of next Food Positio
  , _win :: Bool        -- Whether you reach the 1000 pts
  , _end   :: Bool      -- Whether Game is Over
  , _score  :: Int      -- The Current Score
  , _level :: Int    -- The Game Level
  , _boss :: [Boss]   -- List of Boss
  } deriving (Show)

data Bullet = Bullet
  { _bulletPos :: Pos,        -- Bullet Position
    _bulletDir :: Direction,  -- Bullet Direction
    _friendly :: Bool     
  } deriving (Show)

data Boss = Boss
  { _bossPos :: Pos,     
    _bossForm :: Bool,
    _bossBody1 :: [Pos],
    _bossBody2 :: [Pos],
    _bossLife :: Int,
    _bossTarget :: Pos,
    _bossSleep :: Int
  } deriving (Show, Eq)

data Enemy = Enemy
  { _enPos :: Pos,
    _enLife :: Int,
    _enAlive :: Bool,
    _corpsTime :: Int
  } deriving (Show, Eq)

data Glucose = Glucose
  { _gluPos :: Pos
  } deriving (Show)

-- Generate the Boss information
generateBoss :: Pos -> Boss
generateBoss (V2 x y) = Boss (V2 x y) False b1 b2 bossLife (V2 x y) bossSleepTime
  where 
    b1 = [(V2 (x+1) y), (V2 (x-1) y), (V2 x (y+1)), (V2 x (y-1))]
    b2 = [(V2 (x+1) (y+1)), (V2 (x-1) (y+1)), (V2 (x+1) (y-1)), (V2 (x-1) (y-1))]
   
makeLenses ''GameState


--  Initialize the game information
initState :: IO GameState
initState = do
  -- Randomlize the location of glucoses
  (g :| gs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  enemList <- initEnemyGradual 10  
  let x_m = width `div` 2
      y_m = height `div` 2
      -- Intilize the state
      state  = GameState
        { _bact  = (V2 x_m y_m)
        , _life = 1
        , _playerBulletNum = 0
        , _bullet = []
        , _glucoses = [Glucose (g)]
        , _future_glucoses = gs
        , _win = False
        , _end    = False
        , _enemies = enemList
        , _score  = 0
        , _dir    = E
        , _level   = 1
        , _boss = []
        }
  return $ state

-- Initialize the Enemy information
initEnemyGradual :: Int -> IO [Enemy]
initEnemyGradual 1 = do
  x <- randomRIO (1, width) :: IO Int
  let enem = Enemy (V2 x height) enemLife True 0
  return [enem]
initEnemyGradual n = do
  x <- randomRIO (1, width) :: IO Int
  let y = if (n `mod` 2) == 0 then 0 else height
  let enem = Enemy (V2 x y) enemLife False 0
  l <- initEnemyGradual (n-1)
  return (enem:l)

initEnemySudden :: Int -> IO [Enemy]
initEnemySudden 0 = return []
initEnemySudden n = do
  x <- randomRIO (1, width) :: IO Int
  let y = if (n `mod` 2) == 0 then 0 else height
  let enem = Enemy (V2 x y) enemLife True 0
  l <- initEnemySudden (n-1)
  return (enem:l)


-- Helper Fuction to convert List
fromList :: [a] -> List a
fromList = foldr (:|) (error "Error happens to list")

