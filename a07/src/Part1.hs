module Part1 (run) where

import Data.Function (on)
import Data.List (elemIndex, group, sort, sortBy)
import Data.List.Extra (sortOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))

rankCard :: String -> [Int]
rankCard = map (fromJust . flip elemIndex "23456789TJQKA")

rankHand :: String -> [Int]
rankHand = sortOn Down . map length . group . sort

run :: [(String, Int)] -> Int
run = sum . zipWith (*) [1 ..] . map snd . sortBy (compareHands <> compareCards `on` fst)
  where
    compareHands = compare `on` rankHand
    compareCards = compare `on` rankCard
