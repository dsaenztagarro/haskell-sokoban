module Sokoban where

import Test.HUnit
import Test.QuickCheck
-- Either define Left and Right too so we hide that types
import Prelude hiding (Either(..))
import Data.List (sort)
import Control.Monad (forM)

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
  , wWorker   :: Coord
  , wMax      :: Coord
  , wSteps    :: Int
  } deriving (Show)

emptyWorld = World
  { wWalls    = []
  , wCrates   = []
  , wStorages = []
  , wWorker   = (0,0)
  , wMax      = (0,0)
  , wSteps    = 0
  }

add :: Coord -> Input -> Coord
add (x,y) input =
  case input of
    Up    -> (x, y-1)
    Down  -> (x, y+1)
    Left  -> (x-1, y)
    Right -> (x+1, y)

level = unlines
  ["#####" -- [((0,0),'#'), ((1,0), '#'), ...]
  ,"#.o@#"
  ,"#####"
  ]

loadLevel :: String -> World
loadLevel str = foldl consume (emptyWorld{wMax = maxi}) elems
  -- foldl :: (a -> b -> c) -> a -> [b] -> a
  where lns    = lines str
        coords = [[(x,y) | x <- [0..]] | y <- [0..]]
        elems  = concat $ zipWith zip coords lns
        maxi   = fst . last $ elems
        consume wld (c, elt) =
          case elt of
            '@' -> wld{wWorker = c}
            'o' -> wld{wCrates = c:wCrates wld}
            '#' -> wld{wWalls = c:wWalls wld}
            '.' -> wld{wStorages = c:wStorages wld}
            ' ' -> wld
            otherwise -> error (show elt ++ " not recognized")

displayWorld :: World -> IO ()
displayWorld w = forM coords $ \c -> do
  putStr $ case () of () | isCrate w c && isStorage w c  -> '*'
                         | isWorker w c && isStorage w c -> '+'
                         | isWall w c    -> '#'
                         | isWorker w c  -> '@'
                         | isCrate w c   -> 'o'
                         | isStorage w c -> '.'
                         | otherwise     -> ' '

  where (maxX, maxY)  =  wMax world
        coords        =  [(x,y) | x <- [0..maxX], y <- [0..maxY]]
        isWorker      =  (==)
  -- formM :: Monad m => [a] -> (a -> m b) -> m [b]

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

isStorage :: World -> Coord -> Bool
isStorage world coord = elem coord (wStorages world)

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

main :: IO ()
main = gameLoop $ loadLevel level

gameLoop world = do
  displayWorld world
  input <- getInput
  let world' = if isValid world input
                  then modifyWorld world input
                  else world
  if isFinished world'
    then displayWorld world' >> print "well done!"
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
