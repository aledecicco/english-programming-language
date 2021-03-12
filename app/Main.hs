module Main where

import FuzzyParser (parseProgram)
--import Matcher (matchProgram)

main :: IO ()
main = do
    fc <- readFile "examples/sum.exp"
    (print . parseProgram) fc
