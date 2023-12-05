{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module MyLib (someFunc) where

-- import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as L

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

someFunc :: IO ()
someFunc =
    do 
        parseTest pGame "Game 1: 4 red, 5 blue, 4 green; 7 red, 8 blue, 2 green; 9 blue, 6 red; 1 green, 3 red, 7 blue; 3 green, 7 red"
