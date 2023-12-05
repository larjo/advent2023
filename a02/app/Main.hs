module Main where

import Control.Monad
import Data.Void
import Text.Megaparsec hiding (State, count)
import Data.Text (Text, pack)

import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import MyLib (run)

main :: IO ()
main = run
