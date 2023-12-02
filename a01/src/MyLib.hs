module MyLib (extract) where

import Data.Char

extract :: String -> Int
extract str =
  read [head digits, last digits]
  where
    digits = filter isDigit str
