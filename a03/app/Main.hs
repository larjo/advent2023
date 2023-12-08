module Main where

import qualified MyLib (part1)
import Data.Functor ( (<&>) )

main :: IO ()
main = do
  rows <- readFile "assets/input.txt" <&> lines
  print $ MyLib.part1 rows
