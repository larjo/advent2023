module Part2 (run) where

import Data.List ( elemIndex, group, sort)
import Data.List.Extra (sortOn, maximumOn, groupOn)
import Data.Maybe (fromJust)
import Data.Ord (Down (Down))

rankCard :: String -> [Int]
rankCard =
  map (fromJust . flip elemIndex ranking)
  where
    ranking = "J23456789TQKA"

highest :: (Eq c, Eq b) => [(c, b)] -> c
highest h1 = fst . head $ maximumOn length $ groupOn fst h1

zipped :: [Char] -> [(Char, Int)]
zipped [] = [('J', 1)]
zipped [c] = [(c, 1)]
zipped h = zip (tail h) (prevEq h)

filterJ :: [Char] -> [Char]
filterJ = sort . filter (/= 'J')

highestCard :: String -> Char
highestCard = highest . zipped . filterJ

replaceJ :: String -> String -- replace j with the card that has the highest count.
replaceJ hand = 
  map (\c -> if c == 'J' then high else c) hand
  where
    high = highestCard hand

prevEq :: String -> [Int]
prevEq str = zipWith (\curr prev -> if curr == prev then 1 else 0) str (tail str)

streak :: [Int] -> [Int]
streak = sortOn Down . map (+ 1) . filter (0 /=) . map sum . group

rankHand :: String -> [Int]
rankHand = streak . prevEq . sort . replaceJ

run :: [(String, Int)] -> Int
run input =
  sum . zipWith (\i (_, bet) -> i * bet) [1 ..] $ sortOn (rankHand . fst) . sortOn (rankCard . fst) $ input
