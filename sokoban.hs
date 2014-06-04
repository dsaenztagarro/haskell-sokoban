module Sokoban where

import Test.HUnit
import Test.QuickCheck
-- Either define Left and Right too so we hide that types
import Prelude hiding (Either(..))

data Input
  = Up
  | Down
  | Left
  | Right
  deriving (Show, Eq, Ord)

type Coord = (Int, Int)

data World = World
  { wWalls
  , wCrates
  , wStorages :: [Coord]
  , wWorker :: Coord
  , wSteps :: Int
  } deriving (Show)

emptyWorld = World
  { wWalls = []
  , wCrates = []
  , wStorages = []
  , wWorker = (0,0)
  , wSteps = 0
  }

add :: Coord -> Input -> Coord
add (x,y) input =
  case input of
    Up    -> (x, y-1)
    Down  -> (x, y+1)
    Left  -> (x-1, y)
    Right -> (x+1, y)

loadLevel :: IO World
loadLevel = emptyWorld

displayWorld :: World -> IO ()
displayWorld = print

modifyWorld :: World -> Input -> World
modifyWorld world input = world

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'w' -> return Up
    'a' -> return Left
    's' -> return Down
    'd' -> return Right
    otherwise -> getInput

isWall :: World -> Coord -> Bool
isWall world coord = elem coord (wWalls world)

isCrate :: World -> Coord -> Bool
isCrate world coord = elem coord (wCrates world)

isValid :: World -> Input -> Bool
isValid world input =
  case () of
    ()  | isWall    world newPos -> False
        | isCrate   world newPos ->
          not (isCrate world newPos') && not (isWall world newPos')
        | otherwise        -> True
  where oldPos  = wWorker world
        newPos  = add oldPos input
        newPos' = add newPos input

isFinished :: World -> Bool
isFinished world =
  sort (wCrates world) == sort (wStorages world)

-- all storage spaces covered by crates

main :: IO()
main = gameLoop =<< loadLevel

gameLoop world = do
  input <- getInput
  let world' = if isValid world input
                  then modifyWorld world input
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
