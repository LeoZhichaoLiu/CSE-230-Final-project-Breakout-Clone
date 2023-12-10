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
  , _glucoses   :: [Glucose]       -- Glucose Position
  , _future_glucoses :: List Pos
  , _enemies  :: [Enemy]  -- List of next Food Positio
  , _win :: Bool        -- Whether you reach the 1000 pts
  , _end   :: Bool      -- Whether Game is Over
  , _score  :: Int       -- The Current Score
  , _level :: Int    -- The Game Level
  } deriving (Show)


data Enemy = Enemy
  { _enPos :: Pos,
    _enLife :: Int,
    _enAlive :: Bool
  } deriving (Show)
height, width, enemLife :: Int
height = 40
width = 40
enemLife = 10000

data Glucose = Glucose
  { _gluPos :: Pos
  } deriving (Show)


-- Position for each element
type Pos = V2 Int

data Direction
  = North
  | South
  | East
  | West
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
        --, _glucoses   = [(Glucose (V2 5 15)), (Glucose (V2 6 25))]
        , _glucoses = [Glucose (f)]
        , _future_glucoses = fs
        , _win = False
        , _end    = False
        , _enemies = enemList
        , _score  = 0
        , _dir    = East
        , _level   = 1
        }
  -- Use Monad to input the current state to nextFood Monad Function (continue update state)
  --return $ execState nextFood state
  return $ state

initEnemy :: Int -> IO [Enemy]
initEnemy 0 = return []
initEnemy n = do
  x <- randomRIO (1, width) :: IO Int
  y <- randomRIO (1, width) :: IO Int
  let enem = Enemy (V2 x y) enemLife True
  l <- initEnemy (n-1)
  return (enem:l)

fromList :: [a] -> List a
fromList = foldr (:|) (error "Streams must be infinite")

step :: GameState -> GameState
step g = g & score %~ (+1)
