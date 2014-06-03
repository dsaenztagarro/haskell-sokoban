module Sokoban where

import Test.HUnit
import Test.QuickCheck

type Coord = (Int, Int)

data World = World
  { wWalls
  , wCrate
  , wStorage :: [Coord]
  , wWorker :: Coord
  , wSteps :: Int
  }

data Input

loadLevel :: IO World
loadLevel = undefined

getInput :: IO Input
getInput = undefined

main :: IO()
main = gameLoop =<< loadLevel

gameLoop world = do
  input <- getInput
  let world' = if isValid input
                  then modifyWorld input
                  else world
  displayWorld world'
  if isFinished world'
    then print "well done!"
    else gameLoop world'
-- TESTS

testsSoko = TestList $ map TestCase
  [assertEqual "" 1
                  1
  ]

prop_empty c1 = (c1::Int) == c1

runTests = do
  runTestTT testsSoko
  quickCheck prop_empty
