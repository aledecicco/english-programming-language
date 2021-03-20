module Main where

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )

main :: IO ()
main = do
    fc <- readFile "examples/sum.exp"
    let p = parseProgram fc
        (p', s) = solveProgram p
    -- evaluateProgram p' s
    return ()
