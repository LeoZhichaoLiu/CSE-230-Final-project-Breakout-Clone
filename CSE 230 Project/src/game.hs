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
import System.Random (Random(..), newStdGen)


data GameState = GameState
  { _bact  :: Pos        -- Bacteria Position
  , _dir    :: Direction    -- Current Bacteria Position (to determine what it should go)
  , _food   :: Pos       -- Food Position
  , _foods  :: List Pos  -- List of next Food Positio
  , _end   :: Bool      -- Whether Game is Over
  , _score  :: Int       -- The Current Score
  } deriving (Show)


height, width :: Int
height = 20
width = 20

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

-- Update the next Food position if the bacteria doesn't eat it, and also update the remaining foods list
nextFood :: State GameState ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs
  return ()

--  Initialize the game information
initState :: IO GameState
initState = do
  -- 随机生成所有食物的地方
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      -- 生成state的初始状态
      state  = GameState
        { _bact  = (V2 xm ym)
        , _food   = f
        , _foods  = fs
        , _score  = 0
        , _dir    = East
        }
  -- Use Monad to input the current state to nextFood Monad Function (continue update state)
  --return $ executeState nextFood state
  return $ state


fromList :: [a] -> List a
fromList = foldr (:|) (error "Streams must be infinite")

