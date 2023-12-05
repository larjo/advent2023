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

pBallColor :: Parser BallColor
pBallColor = choice
  [ Red   <$ string "red"
  , Blue  <$ string "blue"
  , Green <$ string "green" ]

data Ball = Ball
  { count :: Int
  , color :: BallColor
  } deriving (Eq, Show)

pBall :: Parser Ball
pBall = Ball <$> (spaceChar *> L.decimal <* spaceChar) <*> pBallColor

data Balls = Balls
  { balls :: [Ball]
  } deriving (Eq, Show)

pBalls :: Parser Balls
pBalls = Balls <$> sepBy pBall (char ',')

data Game = Game
  { number :: Int
  , turn :: [Balls]
  } deriving (Eq, Show)

pGame :: Parser Game
pGame = Game <$> (string "Game" *> spaceChar *> L.decimal <* char ':') <*> sepBy pBalls (char ';')

pGames :: Parser [Game]
pGames = endBy pGame (char '\n')

someFunc :: IO ()
someFunc =
  do
    text <- readFile "assets/input.txt"

    let parsed = parse pGames "" (pack text)

    case parsed of
      Left err -> putStrLn $ errorBundlePretty err
      Right games -> print games


