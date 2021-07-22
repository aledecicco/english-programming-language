module Main where

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram, ReadWrite(..) )
import PrettyPrinter ( ppError )


main :: IO ()
main = do
    fc <- readFile "examples/Flatten.epl"
    case parseProgram fc of
        Left e -> putStrLn e
        Right p ->
            case solveProgram p of
                Left e -> putStrLn $ ppError e
                Right ((p', _), d) -> do
                    r <- evaluateProgram p' d
                    case r of
                        Left e -> putStrLn $ "\n" ++ ppError e
                        Right _ -> putChar '\n'
