{-# LANGUAGE OverloadedStrings #-}

module Part2 (run) where

import Data.Char (isDigit)
import Data.List.Extra (productOn', sumOn')
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T (length, pack, unpack)
import Data.Void
import Text.Megaparsec hiding (State)

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
  unpack $ parse pNumbers "" $ T.pack str
  where
    unpack (Left _) = Nothing
    unpack (Right []) = Nothing
    unpack (Right val) = Just val

numberPositions :: [String] -> [(Int, Int, Text)]
numberPositions = do
  concat . catMaybes . zipWith mergeIndex [0 ..]
  where
    mergePosition i (StringNumber offset value) = (i, offset, value)
    mergeIndex i row = map (mergePosition i) <$> parseLine row

starPositions :: [String] -> [(Int, Int)]
starPositions rows =
  [ (i, j) | (i, row) <- zip [0 ..] rows, (j, chr) <- zip [0 ..] row, chr == '*'
  ]

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (i, j) =
  [ (i', j') | i' <- [i - 1 .. i + 1], j' <- [j - 1 .. j + 1], (i', j') /= (i, j)
  ]

digitPositions :: (Int, Int, Text) -> [(Int, Int)]
digitPositions (row, col, number) =
  [(row, col + i) | i <- [0 .. T.length number - 1]]

isTouched :: [(Int, Int)] -> [(Int, Int)] -> Bool
isTouched positions = any (`elem` positions)

starNumbers :: [(Int, Int, Text)] -> (Int, Int) -> [Text]
starNumbers numbers star =
  mapMaybe
    ( \np@(_r, _c, number) ->
        if isTouched (digitPositions np) $ adjacent star
          then Just number
          else Nothing
    )
    numbers

textToInt :: Text -> Int
textToInt = read . T.unpack

run :: [String] -> Int
run input =
  sumOn' (productOn' textToInt) pairs
  where
    stars = starPositions input
    numbers = numberPositions input
    pairs = filter ((==) 2 . length) $ map (starNumbers numbers) stars
