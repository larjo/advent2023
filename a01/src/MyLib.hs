module MyLib (extract) where

import Data.Char
import Data.List

import Data.Maybe ( mapMaybe, listToMaybe )
import GHC.Base ((<|>))


extract :: String -> Maybe Int
extract str = do
    let digits = stringToDigits str
    first <- listToMaybe digits
    last <- listToMaybe . reverse $ digits
    return $ 10 * first + last

stringToDigits str =
  mapMaybe digitAtHead $ takeWhile (not . null) $ iterate tail str

digitAtHead :: String -> Maybe Int
digitAtHead str =
  firstDigit str <|> firstStringDigit str

firstDigit :: [Char] -> Maybe Int
firstDigit (chr : _) | isDigit chr = Just $ digitToInt chr
firstDigit _ = Nothing

firstStringDigit str =
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
