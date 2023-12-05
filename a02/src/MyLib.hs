{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MyLib (someFunc) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

import Text.Megaparsec
import Data.Text (pack)

type Parser = Parsec Void Text

data BallColor =
    Red
  | Blue
  | Green
  deriving (Eq, Show)

data Ball = Ball
  { count :: Int
  , color :: BallColor
  } deriving (Eq, Show)

data Balls = Balls
  { balls :: [Ball]
  } deriving (Eq, Show)

data Game = Game
  { number :: Int
  , turn :: [Balls]
  } deriving (Eq, Show)

pBallColor :: Parser BallColor
pBallColor = choice
  [ Red   <$ string "red"
  , Blue  <$ string "blue"
  , Green <$ string "green" ]

pBall :: Parser Ball
pBall = do
  _ <- spaceChar
  count <- L.decimal
  _ <- spaceChar
  color <- pBallColor
  return (Ball count color)

pBalls :: Parser Balls
pBalls = Balls <$> sepBy pBall (char ',')

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

someFunc :: IO ()
someFunc =
  do
    text <- readFile "assets/input.txt"
    let parsed = parse pGames "" (pack text)
    case parsed of
      Left err -> putStrLn $ errorBundlePretty err
      Right games -> print games
