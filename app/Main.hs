module Main where

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram )

main :: IO ()
main = do
    fc <- readFile "examples/Sum.epl"
    let p = parseProgram fc
        (p', s) = solveProgram p
    evaluateProgram p' s
    putStr "\n"
    return ()
