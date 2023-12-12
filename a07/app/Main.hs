module Main where

import qualified Part1 (run)
import qualified Part2 (run)

import Data.Functor ( (<&>) )

main :: IO ()
main = do
  rows <- readFile "assets/input.txt" <&> lines
  print $ Part1.run rows -- 251136060
  print $ Part2.run rows -- 249031891, is to low
