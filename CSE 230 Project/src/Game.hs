{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
--import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen, randomRIO)


data GameState = GameState
  { _bact  :: Pos        -- Bacteria Position
  , _dir    :: Direction    -- Current Bacteria Position (to determine what it should go)
  , _life  :: Int   
  , _playerBulletNum :: Int
  , _bullet :: [Bullet]
  , _glucoses   :: [Glucose]       -- Glucose Position
  , _future_glucoses :: List Pos
  , _enemies  :: [Enemy]  -- List of next Food Positio
  , _win :: Bool        -- Whether you reach the 1000 pts
  , _end   :: Bool      -- Whether Game is Over
  , _score  :: Int       -- The Current Score
  , _level :: Int    -- The Game Level
  , _boss :: [Boss]
  } deriving (Show)

data Bullet = Bullet
  { _bulletPos :: Pos,
    _bulletDir :: Direction,
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
  } deriving (Show)

generateBoss :: Pos -> Boss
generateBoss (V2 x y) = Boss (V2 x y) False b1 b2 bossLife (V2 x y) bossSleepTime
  where 
    b1 = [(V2 (x+1) y), (V2 (x-1) y), (V2 x (y+1)), (V2 x (y-1))]
    b2 = [(V2 (x+1) (y+1)), (V2 (x-1) (y+1)), (V2 (x+1) (y-1)), (V2 (x-1) (y-1))]

data Enemy = Enemy
  { _enPos :: Pos,
    _enLife :: Int,
    _enAlive :: Bool,
    _corpsTime :: Int
  } deriving (Show)
height, width, enemLife, corpTime, bossLife :: Int
height = 40
width = 50
enemLife = 80
corpTime = 20
bossLife = 5
bossSleepTime = 11

data Glucose = Glucose
  { _gluPos :: Pos
  } deriving (Show)


-- Position for each element
type Pos = V2 Int

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

makeLenses ''GameState
   

--  Initialize the game information
initState :: IO GameState
initState = do
  -- 随机生成所有食物的地方
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  enemList <- initEnemy 10  
  let xm = width `div` 2
      ym = height `div` 2
      -- 生成state的初始状态
      state  = GameState
        { _bact  = (V2 xm ym)
        , _life = 1
        , _playerBulletNum = 0
        , _bullet = []
        --, _glucoses   = [(Glucose (V2 5 15)), (Glucose (V2 6 25))]
        , _glucoses = [Glucose (f)]
        , _future_glucoses = fs
        , _win = False
        , _end    = False
        , _enemies = enemList
        , _score  = 0
        , _dir    = E
        , _level   = 1
        , _boss = [generateBoss (V2 40 40)]
        }
  -- Use Monad to input the current state to nextFood Monad Function (continue update state)
  --return $ execState nextFood state
  return $ state

initEnemy :: Int -> IO [Enemy]
initEnemy 1 = do
  x <- randomRIO (1, width) :: IO Int
  let enem = Enemy (V2 x height) enemLife True 0
  return [enem]
initEnemy n = do
  x <- randomRIO (1, width) :: IO Int
  let y = if (n `mod` 2) == 0 then 0 else height
  let enem = Enemy (V2 x y) enemLife False 0
  l <- initEnemy (n-1)
  return (enem:l)

fromList :: [a] -> List a
fromList = foldr (:|) (error "Streams must be infinite")

step :: GameState -> GameState
step g = g & score %~ (+1)
