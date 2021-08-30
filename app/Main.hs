module Main where

import System.Console.ANSI
import System.IO.Error ( tryIOError )
import Control.Monad ( unless, guard )

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram )
import PrettyPrinter ( ppError, ppWarning )



-- Messages

message :: Color -> String -> IO ()
message c msg = do
    setSGR [SetColor Foreground Vivid c]
    putStrLn msg
    putStrLn ""
    setSGR [Reset]

errorMessage :: String -> IO ()
errorMessage = message Red

warningMessage :: String -> IO ()
warningMessage = message Yellow

successMessage :: String -> IO ()
successMessage = message Green

--


-- Main

main :: IO ()
main = do
    putStrLn ""
    r <- tryIOError $ readFile "examples/Head.epl"
    case r of
      Left e -> errorMessage "Error: the input file could not be opened."
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

--
