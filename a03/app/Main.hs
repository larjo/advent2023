module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (singleton, tails, transpose)

input :: [String]
input =
  [ "467..114.2",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......7550",
    "...$.*....",
    ".555...777",
    ".664.598.."
  ]

pad :: [String] -> [String]
pad lst@(hd : _) =
  dots ++ lst ++ dots
  where
    dots = [map (const '.') hd]
pad _ = []

padSides :: [String] -> [String]
padSides = transpose . pad . transpose

window :: Int -> [a] -> [[a]]
window m = foldr (zipWith (:)) (repeat []) . take m . tails

mergeSymbols :: [String] -> [Bool]
mergeSymbols = map (any isSym) . transpose
  where
    isSym c = not (isDigit c || c == '.')

makeDigits :: String -> String
makeDigits = map (\c -> if isDigit c then c else '.')

spread :: [Bool] -> [Bool]
spread row =
  map or $
    transpose
      [ tail row ++ singleton False,
        row,
        singleton False ++ init row
      ]

mergeDigits :: [Bool] -> String -> [(Bool, Char)]
mergeDigits = zip

extractNumbers :: [(Bool, Char)] -> [Int]
extractNumbers =
  reverse
    . snd
    . foldl
      ( \((wasTouched, current), numbers) (isTouched, chr) ->
          let nextWasTouched = isTouched || wasTouched in
          if isDigit chr
          then
            let nextNumber = 10 * current + digitToInt chr in
            ((nextWasTouched, nextNumber), numbers)
          else
            if current > 0 && nextWasTouched
              then ((False, 0), current : numbers)
              else ((False, 0), numbers)
      )
      ((False, 0), [])

main :: IO ()
main = do
  let paddedSides = padSides input
  let str = window 3 $ pad paddedSides
  let merged = map mergeSymbols str
  putStrLn "merged"
  mapM_ print merged
  let spr = map spread merged
  putStrLn "spread"
  mapM_ print spr
  let combined = zipWith mergeDigits spr paddedSides
  putStrLn "combined"
  mapM_ print combined
  let extracted = map extractNumbers combined
  putStrLn "extracted"
  mapM_ print extracted
