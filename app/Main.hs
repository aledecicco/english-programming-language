{-|
Module      : Main
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The entry point of the interpreter.
-}

module Main where

import Control.Monad (mapM_, unless, when)
import System.Console.ANSI
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO.Error (tryIOError)

import Errors (Error)
import Evaluator (evaluateProgram)
import FuzzyParser (parseProgram)
import PrettyPrinter (ppError, ppWarning)
import Solver (solveProgram)


-- -----------------
-- * Options

-- | The flags that can be passed via command line.
data Flag =
    Silent -- ^ Don't output warning or success messages. `-s` or `--silent`.
    | Help -- ^ Output a help message. `-h` or `--help`.
    deriving Eq

options :: [OptDescr Flag]
options =
    [
        Option ['s'] ["silent"] (NoArg Silent) "Don't display messages. Only errors and program output are shown.",
        Option ['h'] ["help"] (NoArg Help) "Display this message."
    ]

-- | The help message to show with the `-h` option.
help :: String
help = init $  usageInfo "Usage: stack run -- [-s] file" options

-- | Returns the command line arguments for the interpreter.
getOptions :: IO ([Flag], FilePath)
getOptions = do
    -- Get the arguments.
    (opts, args, errs) <- getOpt Permute options <$> getArgs
    -- If there are misused flags, print errors and exit.
    unless (null errs) $ mapM_ (errorMessage . ppFlagError) errs >> exitFailure
    -- If there isn't exactly one non-flag argument, print an error and exit.
    when (length args /= 1) $ errorMessage "Error: expected one file as argument." >> exitFailure
    return (opts, head args)
    where
        ppFlagError :: String -> String
        ppFlagError err = "Error: " ++ init err ++ "."


-- -----------------
-- * Messages

-- | Prints a message to standard output in the given color.
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

commonMessage :: String -> IO ()
commonMessage = message White

-- | If a computation failed, prints the error message and exits.
-- Otherwise, returns the result.
tryOrExit ::
    Either Error a -- ^ The result or error of a computation.
    -> [String] -- ^ The lines of the source code.
    -> IO a -- ^ The result of the computation.
tryOrExit (Left err) lines = do
    errorMessage $ ppError lines err
    exitFailure
tryOrExit (Right res) _ = return res


-- -----------------
-- * Main

-- | Parses and runs an EPL program.
-- Gets the file path and other options as command line arguments.
main :: IO ()
main = do
    putChar '\n'

    -- Parse command line arguments.
    (opts, filePath) <- getOptions
    when (Help `elem` opts) $ commonMessage help >> exitSuccess
    let silent = Silent `elem` opts

    -- Read the file contents.
    readRes <- tryIOError $ readFile filePath
    fileContent <- case readRes of
        Left _ -> errorMessage "Error: the input file could not be opened." >> exitFailure
        Right str -> return str
    let fileLines = lines fileContent

    -- Parse the program.
    unsolvedProg <- tryOrExit (parseProgram fileContent) fileLines

    -- Solve missing pieces.
    let (solverRes, ws) = solveProgram unsolvedProg
    unless silent $ mapM_ (warningMessage . ppWarning fileLines) (reverse ws)
    ((solvedProg, _), _) <- tryOrExit solverRes fileLines
    unless silent $ successMessage "Parsed successfully."

    -- Evaluate the result.
    evalRes <- evaluateProgram solvedProg
    tryOrExit evalRes fileLines
    putChar '\n'
    exitSuccess
