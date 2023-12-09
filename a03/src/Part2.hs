{-# LANGUAGE OverloadedStrings #-}

module Part2 (run, test2) where

import Data.Char (digitToInt, isDigit)
import Data.List (singleton, tails, transpose)
import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Maybe (mapMaybe, catMaybes)

testInput :: ([String], Int)
testInput =
  ( [ "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    ],
    4361
  )

type Parser = Parsec Void Text

data StringNumber = StringNumber
  { offset :: Int,
    value :: Text
  }
  deriving (Show)

pStringNumber :: Parser StringNumber
pStringNumber = do
  pos <- getOffset
  num <- takeWhile1P Nothing isDigit
  _ <- takeWhileP Nothing (not . isDigit)
  return (StringNumber pos num)

pNumbers :: Parser [StringNumber]
pNumbers = do
  _ <- takeWhileP Nothing (not . isDigit)
  many pStringNumber

parseLine :: String -> Maybe [StringNumber]
parseLine str =
  unpack $ parse pNumbers "" $ pack str
  where
    unpack (Left _) = Nothing
    unpack (Right []) = Nothing
    unpack (Right val) = Just val

test2 :: [[(Int, Int, Text)]]
test2 = do
  catMaybes $ zipWith mergeIndex [0..] (fst testInput)
  where
    mergePosition i (StringNumber offset value) = (i ,offset, value)
    mergeIndex :: Int -> String -> Maybe [(Int, Int, Text)]
    mergeIndex i row =
      map (mergePosition i) <$> parseLine row

run :: [String] -> Int
run input = 123
