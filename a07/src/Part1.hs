module Part1 (run) where

import Data.List ( elemIndex, group, sort, sortBy)
import Data.List.Extra (sortOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))
import Data.Function (on)

rankCard :: String -> [Int]
rankCard = map cardIndex
  where
    cardIndex hand = fromJust $ hand `elemIndex` "23456789TJQKA"

rankHand :: String -> [Int]
rankHand = sortOn Down . filter (/= 1) . map length . group . sort

run :: [(String, Int)] -> Int
run = sum . zipWith (\i (_, bet) -> i * bet) [1 ..] . sortBy (compareHands <> compareCards `on` fst)
  where
    compareHands = compare `on` rankHand
    compareCards = compare `on` rankCard
