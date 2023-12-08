module Main where

import qualified Part1 (run)
import Data.Functor ( (<&>) )

main :: IO ()
main = do
  rows <- readFile "assets/input.txt" <&> lines
  print $ Part1.run rows
