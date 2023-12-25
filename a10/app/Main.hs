module Main where

import Control.Arrow
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import GHC.Exception (divZeroException)
import System.Environment (getArgs)

testInput1 :: [String]
testInput1 =
  [ ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  ]

testInput2 :: [String]
testInput2 =
  [ "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  ]

data Position = Position
  { x :: Int,
    y :: Int,
    d :: Direction,
    grid :: [String]
  }
  deriving (Eq)

instance Show Position where
  show (Position {x, y, d, grid}) = show x ++ " " ++ show y ++ " " ++ show d ++ " " ++ show (getCurrentChar (Position {x, y, d, grid}))

samePos :: Position -> Position -> Bool
samePos (Position {x = x1, y = y1}) (Position {x = x2, y = y2}) = x1 == x2 && y1 == y2

moveTo :: Position -> Direction -> (Int, Int) -> Maybe Position
moveTo (Position {grid}) d (x, y) =
  if x < 0 || y < 0 || y >= length grid || x >= length (grid !! y)
    then Nothing
    else Just $ Position {x, y, d, grid}

getCurrentChar :: Position -> Char
getCurrentChar (Position {x, y, grid}) = (grid !! y) !! x

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq)

charToDirection :: Direction -> Char -> Maybe Direction
charToDirection d c =
  case (d, c) of
    (North, '|') -> Just North
    (North, '7') -> Just West
    (North, 'F') -> Just East
    (East, '-') -> Just East
    (East, 'J') -> Just North
    (East, '7') -> Just South
    (South, '|') -> Just South
    (South, 'J') -> Just West
    (South, 'L') -> Just East
    (West, '-') -> Just West
    (West, 'F') -> Just South
    (West, 'L') -> Just North
    _ -> Nothing

-- findChar :: Char -> [String] -> Maybe Position
findChar :: Char -> [String] -> [(Int, Int)]
findChar c grid =
  [(x, y) | (y, row) <- zip [0 ..] grid, (x, char) <- zip [0 ..] row, char == c]

move :: Position -> Direction -> Maybe Position
move p@(Position {x, y}) d =
  case d of
    North -> moveTo p d (x, y - 1)
    East -> moveTo p d (x + 1, y)
    South -> moveTo p d (x, y + 1)
    West -> moveTo p d (x - 1, y)

step :: Position -> Position
step p@(Position {d}) = fromJust $ move p =<< charToDirection d (getCurrentChar p)

startPositions :: Position -> [Position]
startPositions position =
  filter validDirection $ mapMaybe (move position) [North, East, South, West]
  where
    validDirection p@(Position {d}) = isJust $ charToDirection d $ getCurrentChar p

task1 :: [String] -> IO ()
task1 input = do
  let [(xPos, yPos)] = findChar 'S' input
  let [startPos1,startPos2] = startPositions $ Position xPos yPos East input
  let paths = zip (iterate step startPos1) (iterate step startPos2)
  print $ (+ 1) . length . takeWhile not $ map (uncurry samePos . (fst &&& snd)) paths

main :: IO ()
main = do
  fullInput <- readFile "assets/input.txt" <&> lines
  args <- getArgs
  let input =
        case args of
          ["1"] -> testInput1
          ["2"] -> testInput2
          ["3"] -> fullInput
          _ -> error "Invalid argument"
  task1 input
