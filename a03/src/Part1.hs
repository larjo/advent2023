module Part1 (run, test) where

import Data.Char (digitToInt, isDigit)
import Data.List (singleton, tails, transpose)

testInput :: [String]
testInput =
  [ "^67..114.1",
    "1...*.....",
    "..35..633.",
    "..........",
    ".17.......",
    "$......58.",
    "7.592.....",
    "......755.",
    "...$.*....",
    ".664.598.3"
  ]

pad :: [String] -> [String]
pad lst@(hd : _) =
  dots ++ lst ++ dots
  where
    dots = [map (const '.') hd]
pad [] = []

padSides :: [String] -> [String]
padSides = transpose . pad . transpose

window :: Int -> [a] -> [[a]]
window m = foldr (zipWith (:)) (repeat []) . take m . tails

mergeSymbols :: [String] -> [Bool]
mergeSymbols = map (any isSym) . transpose
  where
    isSym c = not (isDigit c || c == '.')

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
            if current > 0 && wasTouched
              then ((False, 0), current : numbers)
              else ((False, 0), numbers)
      )
      ((False, 0), [])

test :: IO ()
test = do
  let paddedSides = padSides testInput
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
  print $ sum . map sum $ extracted

run :: [String] -> Int
run rows =
  sum (map (sum . extractNumbers) zipped)
  where
    paddedSides = padSides rows
    symbols = map (spread . mergeSymbols) (window 3 $ pad paddedSides)
    zipped = zipWith mergeDigits symbols paddedSides
