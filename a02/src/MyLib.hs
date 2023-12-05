{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MyLib (run) where

import Control.Monad
import Data.Data (isAlgType)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State, count)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

data BallColor
  = Red
  | Blue
  | Green
  deriving (Eq, Show)

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
  color <- pBallColor
  return (Ball count color)

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
allowedGames = filter (isGameAllowed bag)
  where
    -- 12 red cubes, 13 green cubes, and 14 blue cubes
    bag = [Ball 12 Red, Ball 13 Green, Ball 14 Blue]

run :: IO ()
run =
  do
    text <- readFile "assets/input.txt"
    let parsed = parse pGames "" (pack text)
    case parsed of
      Left err -> putStrLn $ errorBundlePretty err
      Right games ->
        putStrLn $ show . sum . map number $ allowedGames games
