-- module Test where
-- import Test.Tasty
-- import Test.Tasty.HUnit
-- import Test.HUnit
-- import Control.Lens



import Test.HUnit
import Setting
import Game


main :: IO ()
main = do
  runTestTT initStateTest
  return ()

-- Test the initState function
initStateTest :: Test
initStateTest = "Test initState" ~: do
  initialState <- initState
  let expectedState = GameState
        { _bact = V2 (width `div` 2) (height `div` 2)
        , _life = 1
        , _glucoses = [Glucose (V2 0 0)]  -- Assuming randomRs generates (V2 0 0)
        , _future_glucoses = List []  -- Assuming randomRs generates an empty list for the future glucoses
        , _win = False
        , _end = False
        , _enemies = replicate 10 (Enemy (V2 0 0) enemLife True)  -- Assuming initEnemy generates enemies at (V2 0 0)
        , _score = 0
        , _dir = East
        , _level = 1
        }
  assertEqual "Initial state should match expected state" expectedState initialState