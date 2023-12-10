{-# LANGUAGE OverloadedStrings #-}

module MyLib (part1, part2) where

import Data.List.Extra (sumOn')
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State, count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data BallColor
  = Red
  | Blue
  | Green
  deriving (Eq, Show)

colors :: [BallColor]
colors = [Red, Green, Blue]

data Ball = Ball
  { count :: Int,
    color :: BallColor
  }
  deriving (Eq, Show)

data Game = Game
  { number :: Int,
    turn :: [[Ball]]
  }
  deriving (Eq, Show)

pBallColor :: Parser BallColor
pBallColor =
  choice
    [ Red <$ string "red",
      Blue <$ string "blue",
      Green <$ string "green"
    ]

pBall :: Parser Ball
pBall = do
  _ <- spaceChar
  count <- L.decimal
  _ <- spaceChar
  Ball count <$> pBallColor

pBalls :: Parser [Ball]
pBalls = sepBy pBall (char ',')

pGame :: Parser Game
pGame = do
  _ <- string "Game"
  _ <- spaceChar
  number <- L.decimal
  _ <- char ':'
  turn <- sepBy pBalls (char ';')
  return (Game number turn)

pGames :: Parser [Game]
pGames = endBy pGame eol

isBallAllowed :: [Ball] -> Ball -> Bool
isBallAllowed bag ball =
  any (\b -> color ball == color b && count ball <= count b) bag

isBallsAllowed :: [Ball] -> [Ball] -> Bool
isBallsAllowed bag = all (isBallAllowed bag)

isGameAllowed :: [Ball] -> Game -> Bool
isGameAllowed bag game = all (isBallsAllowed bag) $ turn game

allowedGames :: [Game] -> [Game]
allowedGames =
  filter (isGameAllowed bag)
  where
    bag = [Ball 12 Red, Ball 13 Green, Ball 14 Blue]

countByColor :: BallColor -> [Ball] -> Int
countByColor ballcolor = sumOn' (\b -> if color b == ballcolor then count b else 0)

maxByColor :: [Ball] -> [Ball] -> BallColor -> Ball
maxByColor b1 b2 ballcolor =
  Ball maxCount ballcolor
  where
    maxCount = max (countByColor ballcolor b1) (countByColor ballcolor b2)

maxGame :: [Ball] -> [Ball] -> [Ball]
maxGame b1 b2 = map (maxByColor b1 b2) colors

game0 :: [Ball]
game0 = map (Ball 0) colors

power :: [Ball] -> Int
power = product . map count

part1 :: IO ()
part1 =
  do
    text <- readFile "assets/input.txt"
    let parsed = parse pGames "" (pack text)
    case parsed of
      Left err -> putStrLn $ errorBundlePretty err
      Right games ->
        print $ sum . map number $ allowedGames games

part2 :: IO ()
part2 =
  do
    text <- readFile "assets/input.txt"
    let parsed = parse pGames "" (pack text)
    case parsed of
      Left err -> putStrLn $ errorBundlePretty err
      Right games ->
        print $ sum . map (power . foldr maxGame game0 . turn) $ games
