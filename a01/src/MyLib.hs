module MyLib (extract) where

import Data.Char
import Data.List
import Data.Maybe
import GHC.Base ((<|>))

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
  digit str <|> digitString str

digit (chr : _) =
  if isDigit chr
    then Just $ digitToInt chr
    else Nothing
digit [] = Nothing

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
