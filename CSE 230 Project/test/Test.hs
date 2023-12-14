
import Linear.V2 (V2(..), _x, _y)
import Test.Tasty
import Test.Tasty.HUnit
import Game 
import Handler
import Utils
import UI
import System.Random

boss1 = generateBoss (V2 1 2) 
testBoss :: TestTree
testBoss = testCase "Boolean value for initializing Boss" $ do
  assertEqual "Should be false" (_bossForm boss1) False
  assertEqual "Should be 10" (_bossLife boss1) 10
  assertEqual "Should be 11" (_bossSleep boss1) 11

initStateTest :: TestTree
initStateTest = testCase "Test initState" $ do
  intstate <- initState
  assertEqual "Initial state should match expected state" (_life intstate) 1
  assertEqual "Initial state should match expected state" (_win intstate) False
  assertEqual "Initial state should match expected state" (_end intstate) False
  assertEqual "Initial state should match expected state" (_score intstate) 0
  assertEqual "Initial state should match expected state" (_dir intstate) E
  assertEqual "Initial state should match expected state" (_level intstate) 1
  assertEqual "Initial state should match expected state" (_bact intstate) (V2 25 20)

initEnemyGradualTest:: TestTree
initEnemyGradualTest = testCase "Test initEnemyGradual" $ do
  let len1 =  1
  enemies1 <- initEnemyGradual len1
  assertEqual "Base case: The length should be equal to the input" (length enemies1) len1

  let len1 = 9865
  enemies1 <- initEnemyGradual len1
  assertEqual "large input: The length should be equal to the input" (length enemies1) len1

chaseXTest:: TestTree
-- only change the position along x-axes
chaseXTest = testCase "Test ChaseX" $ do 
  let iniPos = V2 10 10
      bact = V2 100 11
      nextPos = chaseX iniPos bact
  assertEqual "The next postion should be 11 10" nextPos (V2 11 10)

  let iniPos = V2 44 10
      bact = V2 10 11
      nextPos = chaseX iniPos bact
  assertEqual "The next postion should be 43 10" nextPos (V2 43 10)

  let iniPos = V2 5 10
      bact = V2 5 11
      nextPos = chaseX iniPos bact
  assertEqual "The next postion should be 5 10" nextPos (V2 5 10)


chaseYTest:: TestTree
-- only change the position along x-axes
chaseYTest = testCase "Test ChaseY" $ do 
  let iniPos = V2 23 16
      bact = V2 23 11
      nextPos = chaseY iniPos bact
  assertEqual "The next postion should be 23 15" nextPos (V2 23 15)

  let iniPos = V2 33 18
      bact = V2 33 23
      nextPos = chaseY iniPos bact
  assertEqual "The next postion should be 33 19" nextPos (V2 33 19)

  let iniPos = V2 8 11
      bact = V2 8 11
      nextPos = chaseY iniPos bact
  assertEqual "The next postion should be 8 11" nextPos (V2 8 11)


isEnemyAtPosTest:: TestTree
isEnemyAtPosTest = testCase "Test isEnemyAtPos" $ do 
  let test = (V2 10 3)
      enemy1 = Enemy {_enPos = (V2 1 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy2 = Enemy {_enPos = (V2 2 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      atPos = isEnemyAtPos test [enemy1, enemy2]
  assertEqual "The enemy is not at the position" atPos False

  let test = (V2 10 3)
      atPos = isEnemyAtPos test []
  assertEqual "empty list" atPos False

  let test = (V2 10 3)
      enemy1 = Enemy {_enPos = (V2 10 3), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy2 = Enemy {_enPos = (V2 2 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy3 = Enemy {_enPos = (V2 11 1), _enLife = 2, _enAlive = False, _corpsTime = 1}
      atPos = isEnemyAtPos test [enemy1, enemy2]
  assertEqual "The enemy is at the position" atPos True

isCorpAtPosTest:: TestTree
isCorpAtPosTest = testCase "Test isCorpAtPos" $ do 
  let test = V2 12 16
      enemy1 = Enemy {_enPos = (V2 10 3), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy2 = Enemy {_enPos = (V2 2 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy3 = Enemy {_enPos = (V2 11 1), _enLife = 2, _enAlive = False, _corpsTime = 1}
      enelis = [enemy1, enemy2, enemy3]
      (updateEnemies, hit) = isCorpAtPos test enelis
  assertEqual "The position does not match" hit False
  assertEqual "The updated list should be the same" updateEnemies enelis


  let test = V2 12 16
      enemy1 = Enemy {_enPos = (V2 12 16), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy2 = Enemy {_enPos = (V2 2 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy3 = Enemy {_enPos = (V2 11 1), _enLife = 2, _enAlive = False, _corpsTime = 1}
      enelis = [enemy1, enemy2, enemy3]
      (updateEnemies, hit) = isCorpAtPos test enelis
  assertEqual "The position matches, but enemy is alive, so does not hit" hit False


  let test = V2 12 16
      enemy1 = Enemy {_enPos = (V2 12 16), _enLife = 1, _enAlive = False, _corpsTime = 1}
      enemy2 = Enemy {_enPos = (V2 2 1), _enLife = 1, _enAlive = True, _corpsTime = 1}
      enemy3 = Enemy {_enPos = (V2 11 1), _enLife = 2, _enAlive = False, _corpsTime = 1}
      enelis = [enemy1, enemy2, enemy3]
      (updateEnemies, hit) = isCorpAtPos test enelis
  assertEqual "The position matches, but enemy is dead, so does hit" hit True

updateEnemyLifeTest :: TestTree
updateEnemyLifeTest = testCase "test update enemy life "$ do
  let enemy1 = Enemy {_enPos = (V2 12 16), _enLife = 1, _enAlive = False, _corpsTime = -1}
  updatedEnemy <- updateEnemyLife enemy1
  assertEqual "Test enemy that is not alive and negative corpstime" updatedEnemy enemy1

  let testPos = (V2 12 16)
      enemy1 = Enemy {_enPos = testPos , _enLife = 1, _enAlive = False, _corpsTime = 0}
  updatedEnemy <- updateEnemyLife enemy1
  assertBool "The position should be different" (_enPos updatedEnemy /= testPos)
  assertBool "The corpsTime should be less than 0" (_corpsTime updatedEnemy < 0)

  let testPos = (V2 12 16)
      time = 4
      enemy1 = Enemy {_enPos = testPos , _enLife = 1, _enAlive = False, _corpsTime = time}
  updatedEnemy <- updateEnemyLife enemy1
  assertEqual "Test enemy corstime should be time-1" (_corpsTime updatedEnemy) (time-1)
  assertEqual "It is dead" (_enAlive updatedEnemy) False

reviveEnemyTest :: TestTree
reviveEnemyTest = testCase "Test revive enemy" $ do
  let testPos = (V2 12 16)
      enemy1 = Enemy {_enPos = testPos , _enLife = 1, _enAlive = False, _corpsTime = -1}
      enemy2 = Enemy {_enPos = testPos , _enLife = 2, _enAlive = True, _corpsTime = 0}
      enemies = [enemy1, enemy2]
      expectedEnemy = Enemy {_enPos = testPos , _enLife = enemLife, _enAlive = True, _corpsTime = 0}
  assertEqual "Test the if the enemy is revived" (head (reviveEnemy enemies)) expectedEnemy
  assertEqual "If input is empty, then the output should be empty as well" (reviveEnemy []) []

setEnemyDeadTest:: TestTree
setEnemyDeadTest = testCase "Test set enemy dead" $ do
  let p = V2 7 16
      p1 = V2 8 16
      enemy1 = Enemy {_enPos = p , _enLife = 1, _enAlive = True, _corpsTime = -1}
      enemy2 = Enemy {_enPos = p1 , _enLife = 2, _enAlive = True, _corpsTime = 0}
      enemies = [enemy1, enemy2]
      result = setEnemyDead p enemies

      expected1 = Enemy {_enPos = p , _enLife = 1, _enAlive = False, _corpsTime = 20}
      expected2 = Enemy {_enPos = p1 , _enLife = 2, _enAlive = True, _corpsTime = 0}
      exp = [expected1, expected2]
  assertEqual "Test list should match" result exp

addEnemyTest:: TestTree
addEnemyTest = testCase "Test add enemy" $ do
  let p = V2 7 16
      enemy1 = Enemy {_enPos = p , _enLife = 1, _enAlive = True, _corpsTime = -1}
      enemy2 = Enemy {_enPos = p , _enLife = 2, _enAlive = True, _corpsTime = 0}
      enemies = [enemy1, enemy2]
  result <- addEnemy 0 enemies
  assertEqual "The length should not change" (length result) 2

  result <- addEnemy 10 []
  assertEqual "The length should not change" (length result) 10
  assertBool "All enemy should be alive" (all _enAlive result)
  assertEqual "All enemies should have same life" enemLife (_enLife (head result))


moveBulletsTest :: TestTree
moveBulletsTest = testCase "Test move Bullet" $ do 
  let bullet1 = Bullet {_bulletPos = V2 5 5, _bulletDir = N, _friendly = True}
      res = moveBullets [bullet1]
  assertEqual "it should move up 1" (_bulletPos (head res)) (V2 5 6)

  let bullet1 = Bullet {_bulletPos = V2 5 400, _bulletDir = N, _friendly = True}
      res = moveBullets [bullet1]
  assertEqual "Empty list" (length res) 0

  let bullet1 = Bullet {_bulletPos = V2 5 100, _bulletDir = N, _friendly = True}
      bullet2 = Bullet {_bulletPos = V2 3 2, _bulletDir = S, _friendly = True}
      res = moveBullets [bullet1, bullet2]
  assertEqual "The length should be 1" (length res) 1
  assertEqual "it should move up 1" (_bulletPos (head res)) (V2 3 1)

generateBulletTest :: TestTree
generateBulletTest = testCase "Test generate bullet" $ do 
  let p = V2 10 10
      bullets = generateBullet1 True p
      bullets1 = generateBullet1 False p
      b2 = generateBullet2 True p
      b3 = generateBullet2 False p
  assertEqual "The length should be 4" (length bullets) 4
  assertEqual "The length should be 4" (length bullets1) 4
  assertEqual "The length should be 4" (length b2) 4
  assertEqual "The length should be 4" (length b3) 4

bossShootTest :: TestTree
bossShootTest = testCase "Test boss shoot" $ do
  let boss1 = Boss{ _bossPos = (V2 3 3), _bossForm = True, _bossBody1 = [], _bossBody2 = [], _bossLife = 10, _bossTarget = (V2 2 2), _bossSleep = 10}
      boss2 = Boss{ _bossPos = (V2 4 8), _bossForm = True, _bossBody1 = [], _bossBody2 = [], _bossLife = 10, _bossTarget = (V2 2 2), _bossSleep = 0}
      res = bossShoot [boss1, boss2]
  assertEqual "The length should be 4" (length res) 4

  let boss1 = Boss{ _bossPos = (V2 3 3), _bossForm = True, _bossBody1 = [], _bossBody2 = [], _bossLife = 10, _bossTarget = (V2 2 2), _bossSleep = 10}
      boss2 = Boss{ _bossPos = (V2 4 8), _bossForm = True, _bossBody1 = [], _bossBody2 = [], _bossLife = 10, _bossTarget = (V2 2 2), _bossSleep = 10}
      res = bossShoot [boss1, boss2]
  assertEqual "The length should be 0" (length res) 0

isBulletAtPosFriendlyTest:: TestTree
isBulletAtPosFriendlyTest = testCase "Test isBulletAtPosFriendly" $ do
  let p = V2 2 2
      p1 = V2 8 8
      bullet1 = Bullet {_bulletPos = p, _bulletDir = N, _friendly = True}
      bullet2 = Bullet {_bulletPos = V2 3 2, _bulletDir = S, _friendly = True}
      res = isBulletAtPosFriendly p [bullet1, bullet2]
  assertEqual "It should be true" res True

  let p = V2 2 2
      p1 = V2 8 8
      bullet1 = Bullet {_bulletPos = p1, _bulletDir = N, _friendly = True}
      bullet2 = Bullet {_bulletPos = p, _bulletDir = S, _friendly = False}
      res = isBulletAtPosFriendly p [bullet1, bullet2]
  assertEqual "It should be False" res False

  let p = V2 2 2
      p1 = V2 8 8
      bullet1 = Bullet {_bulletPos = p, _bulletDir = W, _friendly = True}
      bullet2 = Bullet {_bulletPos = p, _bulletDir = S, _friendly = True}
      res = isBulletAtPosFriendly (V2 3 3) [bullet1, bullet2]
  assertEqual "It should be False" res False


bulletKillPlayerTest:: TestTree
bulletKillPlayerTest = testCase "Test bulletKillerPlayer" $ do 
  let p = V2 18 9
      bullet1 = Bullet {_bulletPos = p, _bulletDir = W, _friendly = True}
      bullet2 = Bullet {_bulletPos = V2 2 2, _bulletDir = S, _friendly = True}
      res =  bulletKillPlayer p [bullet1, bullet2]
      res1 =  bulletKillPlayer p []
  assertEqual "It should be False" res False
  assertEqual "It should be False" res1 False

  let p = V2 18 9
      bullet1 = Bullet {_bulletPos = p, _bulletDir = W, _friendly = False}
      bullet2 = Bullet {_bulletPos = V2 2 2, _bulletDir = S, _friendly = True}
      res =  bulletKillPlayer p [bullet1, bullet2]
  assertEqual "Bullet is at the position, and it is not friendly" res True


bulletKillBossTest :: TestTree
bulletKillBossTest = testCase "Test bullet kill boss" $ do
  let p = V2 16 4
      bullet1 = Bullet {_bulletPos = p, _bulletDir = W, _friendly = True}
      boss2 = Boss{ _bossPos = p, _bossForm = True, _bossBody1 = [], _bossBody2 = [], _bossLife = 10, _bossTarget = (V2 2 2), _bossSleep = 10}
      res = bulletKillBoss [bullet1] [boss1] 
  assertEqual "Boss should increase 1 health for sleep" (_bossSleep (head res)) 11
  assertEqual "The postion should be different" (_bossPos (head res) /= p) True
  assertEqual "The boss body should not be empty" (length (_bossBody1 (head res))) 4
  assertEqual "The bossLife does not change" (_bossLife (head res)) 10


     



-- Grouping test cases
testGroup1 :: TestTree
testGroup1 = testGroup "MyTestGroup" l
  where l = 
                                      [testBoss,
                                      initStateTest, 
                                      initEnemyGradualTest, 
                                      chaseXTest, 
                                      chaseYTest, 
                                      isEnemyAtPosTest,
                                      isCorpAtPosTest,
                                      updateEnemyLifeTest,
                                      reviveEnemyTest,
                                      setEnemyDeadTest,
                                      moveBulletsTest,
                                      generateBulletTest,
                                      bossShootTest,
                                      isBulletAtPosFriendlyTest,
                                      bulletKillPlayerTest,
                                      bulletKillBossTest]


main = do

  defaultMain testGroup1
  return ()


