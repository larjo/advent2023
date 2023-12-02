module Main where

import MyLib (extract)
import System.IO

main :: IO ()
main = do
  text <- readFile "assets/input.txt"
  let result = sum $ map extract $ lines text
  putStrLn $ "The result is: " ++ show result
