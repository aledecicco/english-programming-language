{-|
Module      : Main
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The entry point of the interpreter.
-}

module Main where

import System.Console.ANSI
import System.IO.Error ( tryIOError )
import System.Exit
import Control.Monad ( unless, guard )

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator ( evaluateProgram )
import PrettyPrinter ( ppError, ppWarning )
import Errors ( Error )


-- -----------------
-- * Messages

-- | Prints a message to standard output in the given color
message :: Color -> String -> IO ()
message color msg = do
    setSGR [SetColor Foreground Vivid color]
    putStrLn msg
    putChar '\n'
    setSGR [Reset]

errorMessage :: String -> IO ()
errorMessage = message Red

warningMessage :: String -> IO ()
warningMessage = message Yellow

successMessage :: String -> IO ()
successMessage = message Green


-- -----------------
-- * Main

-- | Parses and runs an EPL program
-- Gets the file path and other options as command line arguments
main :: IO ()
main = do
    putChar '\n'

    -- Read the file contents
    readRes <- tryIOError $ readFile "examples/Head.epl"
    fc <- case readRes of
        Left _ -> errorMessage "Error: the input file could not be opened." >> exitFailure
        Right str -> return str
    let fileLines = lines fc

    -- Parse the program
    unsolvedProg <- tryOrExit (parseProgram fc) fileLines

    -- Solve missing pieces
    let (solverRes, ws) = solveProgram unsolvedProg
    mapM_ (warningMessage . ppWarning fileLines) $ reverse ws
    ((solvedProg, _), solverData) <- tryOrExit solverRes fileLines
    successMessage "Parsed successfully."

    -- Evaluate the result
    evalRes <- evaluateProgram solvedProg solverData
    tryOrExit evalRes fileLines
    putChar '\n'
    exitSuccess

    where
        -- | If a computation failed, prints the error message and exits
        -- Otherwise, returns the result
        tryOrExit :: Either Error a -> [String] -> IO a
        tryOrExit (Left err) lines = do
            errorMessage $ ppError lines err
            exitFailure
        tryOrExit (Right res) _ = return res

