module Main where

import Parser (parseProgram)
import Matcher (matchProgram)

main :: IO ()
main = do
    fc <- readFile "examples/sum.exp"
    (print . parseProgram) fc
