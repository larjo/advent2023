module Part1 (run) where

import Data.List ( elemIndex, group, sort, sortBy)
import Data.List.Extra (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Function (on)

splitInput :: String -> (String, Int)
splitInput = split . splitOn " "
  where
    split [hand, bet] = (hand, read bet)
    split _ = ("", 0)

rankCard :: String -> [Int]
rankCard = map cardIndex
  where
    cardIndex hand = fromJust $ hand `elemIndex` "23456789TJQKA"

rankHand :: String -> [Int]
rankHand = sortOn Down . filter (/= 1) . map length . group . sort

run :: [String] -> Int
run input =
  sum . sortByRank $ map splitInput input
  where
    sortByRank hands = zipWith (\i (_, bet) -> i * bet) [1 ..] $ sortBy (compareHands <> compareCards `on` fst) hands
    compareHands = compare `on` rankHand
    compareCards = compare `on` rankCard
