-- module Main where
-- import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.HUnit
-- import Control.Lens


import Test.Tasty
import Test.Tasty.HUnit

-- A simple function to test
add :: Int -> Int -> Int
add a b = a + b

-- Test case using HUnit
testCase1 :: TestTree
testCase1 = testCase "Adding two numbers" $
  assertEqual "Should be equal" (add 2 3) 4

-- Grouping test cases
testGroup1 :: TestTree
testGroup1 = testGroup "MyTestGroup" [testCase1]

-- -- Main test suite
-- main :: IO ()


main = do
  print("sss")
  
  defaultMain testGroup1
  -- runTestTT baseTest
  return ()
-- main = defaultMain testGroup1

-- Test the initState function
-- initStateTest :: Test
-- initStateTest = "Test initState" ~: do
--   initialState <- initState
--   let expectedState = GameState
--         { _bact = V2 (width `div` 2) (height `div` 2)
--         , _life = 1
--         , _glucoses = [Glucose (V2 0 0)]  -- Assuming randomRs generates (V2 0 0)
--         , _future_glucoses = List []  -- Assuming randomRs generates an empty list for the future glucoses
--         , _win = False
--         , _end = False
--         , _enemies = replicate 10 (Enemy (V2 0 0) enemLife True)  -- Assuming initEnemy generates enemies at (V2 0 0)
--         , _score = 0
--         , _dir = East
--         , _level = 1
--         }
--   assertEqual "Initial state should match expected state" expectedState initialState