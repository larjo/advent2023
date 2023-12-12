module Part1 (run) where

import Data.List ( elemIndex, group, sort)
import Data.List.Extra (sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))

testInput :: [String]
testInput =
  [ "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  ]

allHands :: [String]
allHands =
  [ "22222", -- Five of a kind, rank = [5]
    "A3333", -- Four of a kind, rank = [4]
    "222TT", -- Full house, rank = [3,2]
    "3555K", -- Three of a kind, rank = [3]
    "33QQA", -- Two pair, rank = [2,2]
    "T2155", -- One pair, rank = [2]
    "32A5J" -- High card, rank = []
  ]

splitInput :: String -> (String, Int)
splitInput = split . splitOn " "
  where
    split [hand, bet] = (hand, read bet)
    split _ = ("", 0)

rankCard :: String -> [Int]
rankCard =
  map (fromMaybe (-1) . flip elemIndex ranking)
  where
    ranking = "23456789TJQKA"

rankHand :: String -> [Int]
rankHand =
  streak . prevEq . sort
  where
    prevEq str = zipWith (\curr prev -> if curr == prev then 1 else 0) str (tail str)
    streak = sortOn Down . map (+ 1) . filter (0 /=) . map sum . group

run :: [String] -> Int
run input =
  sum . sortByRank $ map splitInput input
  where
    sortByRank hands = zipWith (\i (_, bet) -> i * bet) [1 ..] $ sortOn (rankHand . fst) . sortOn (rankCard . fst) $ hands
