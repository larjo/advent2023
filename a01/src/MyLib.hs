module MyLib (extract) where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

extract :: String -> Int
extract str =
  10 * a + b
  where
    a = head digits
    b = last digits
    digits = stringToDigits str

stringToDigits str =
  mapMaybe digitAtHead $ takeWhile (not . null) $ iterate tail str

digitAtHead :: String -> Maybe Int
digitAtHead str =
  digit str `mplus` digitString str

digit str =
  if isDigit chr then Just $ digitToInt chr else Nothing
  where
    chr = head str

digitString str =
  findIndex (`isPrefixOf` str) digitStrings
  where
    digitStrings =
      [ "zero",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine"
      ]
