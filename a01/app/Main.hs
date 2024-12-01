module Main where

import MyLib (extract)
import System.IO
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  text <- readFile "assets/input.txt"
  let result = sum $ mapMaybe extract $ lines text
  putStrLn $ "The result is: " ++ show result
