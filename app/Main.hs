module Main where

import System.IO.Error ( isDoesNotExistError )
import Control.Exception ( tryJust )
import Control.Monad ( unless, guard )

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram )
import PrettyPrinter ( errorMessage, warningMessage, ppError, ppWarning, successMessage )

main :: IO ()
main = do
    putStrLn ""
    r <- tryJust (guard . isDoesNotExistError) $ readFile "examples/Head.epl"
    case r of
      Left e -> errorMessage "Error: the input file does not exist."
      Right fc -> runInterpreter fc
    where
        runInterpreter :: String -> IO ()
        runInterpreter fc = do
            let fLs = lines fc
            case parseProgram fc of
                Left e -> errorMessage $ ppError fLs e
                Right p ->
                    case solveProgram p of
                        (Left e', ws) -> do
                            mapM_ (warningMessage . ppWarning fLs) $ reverse ws
                            errorMessage $ ppError fLs e'
                        (Right ((p', _), d), ws) -> do
                            mapM_ (warningMessage . ppWarning fLs) $ reverse ws
                            successMessage "Parsed successfully."
                            r <- evaluateProgram p' d
                            case r of
                                Left e'' -> errorMessage $ ppError fLs e''
                                Right _ -> putChar '\n'
