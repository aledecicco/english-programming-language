module Main where

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram )
import PrettyPrinter ( ppError, ppWarning )


main :: IO ()
main = do
    fc <- readFile "examples/Head.epl"
    case parseProgram fc of
        Left e -> putStrLn $ ppError e
        Right p ->
            case solveProgram p of
                (Left e', ws) -> do
                    mapM_ (putStrLn . ppWarning) ws
                    putStrLn $ ppError e'
                (Right ((p', _), d), ws) -> do
                    mapM_ (putStrLn . ppWarning) ws
                    r <- evaluateProgram p' d
                    case r of
                        Left e'' -> putStrLn $ ppError e''
                        Right _ -> putChar '\n'
