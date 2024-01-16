module Main where

import Control.Arrow
import Data.Functor ((<&>))
import Data.List
import Data.Maybe
import System.Environment (getArgs)

testInput1 :: [String]
testInput1 =
  [ ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  ]

testInput1b :: [String]
testInput1b =
  [ "--.|.",
    ".S-7|",
    ".||||",
    "-L-J|",
    ".---."
  ]

testInput2 :: [String]
testInput2 =
  [ "..F7.",
    ".FJ|.",
    "SJ.L7",
    "|F--J",
    "LJ..."
  ]

testInput3 :: [String]
testInput3 =
  [ "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  ]

testInput4 :: [String]
testInput4 =
  [ "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
  ]

-- Upper part/lower part translation
-- input: | L 7 F J
-- upper: | |     |
-- lower: |   | |  
-- (upper, lower)

split :: Char -> (Char, Char)
split c =
  case c of
    '|' -> ('|', '|')
    'L' -> ('|', ' ')
    'J' -> ('|', ' ')
    '7' -> (' ', '|')
    'F' -> (' ', '|')
    '.' -> ('.', '.')
    '-' -> ('-', '-')
    _ -> error "Invalid char"

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

setCurrentChar :: Position -> Char -> Position
setCurrentChar (Position {x, y, d, grid}) c =
  Position {x, y, d, grid = replace y (replace x c (grid !! y)) grid}
  where
    replace i e l = take i l ++ [e] ++ drop (i + 1) l

setXY :: Position -> Int -> Int -> Position
setXY (Position {d, grid}) x y = Position {x, y, d, grid}

clearPosition :: Position -> Position
clearPosition (Position {x, y, d, grid}) =
  Position {x, y, d, grid = clearGrid}
  where
    clearGrid =
        map (\s -> replicate (length s) '.') grid

data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)

sReplace :: Direction -> Direction -> Char
sReplace d1 d2 =
  case sort [d1, d2] of
    [North, East] -> 'L'
    [North, South] -> '|'
    [North, West] -> 'J'
    [East, South] -> 'F'
    [East, West] -> '-'
    [South, West] -> '7'
    _ -> error "Invalid direction"

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

cleanedGrid :: [String] -> Position
cleanedGrid input = do
  let [(xPos, yPos)] = findChar 'S' input
  let sPos = Position xPos yPos East input
  let [startPos1, startPos2] = startPositions sPos
  let path = takeWhile (not . samePos sPos) $ iterate step startPos1
  let cleaned = foldr fstep (clearPosition sPos) path
  let sReplacement = sReplace (d startPos1) (d startPos2)
  setCurrentChar (setXY cleaned xPos yPos) sReplacement
  where
    fstep pos state =
      setCurrentChar (setXY state (x pos) (y pos)) (getCurrentChar pos)

countChar :: (Bool, Int) -> Char -> (Bool, Int)
countChar (ins, cnt) c
    | c == '|' = (not ins, cnt)
    | ins && c == '.' = (ins, cnt + 1)
    | otherwise = (ins, cnt)

countRow :: [Char] -> Int
countRow = snd . foldl countChar (False, 0)

task2 :: [String] -> IO ()
task2 input = do
  let cleaned = cleanedGrid input
  let sp = map (map split) $ grid cleaned
  let sp1 = map (map fst) sp
  let sp2 = map (map snd) sp
  let c1 = map countRow sp1
  let c2 = map countRow sp2
  print $ sum c1
  print $ sum c2

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
  task2 input