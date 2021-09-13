{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : EvaluatorTest
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Tests for the "Evaluator" that run the programs in the examples folder.
-}

module ExamplesTest (tests) where

import Control.Monad.Trans.State (gets, modify, runState, State)
import Data.Bifunctor (first, second)
import System.IO.Error (tryIOError)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), Assertion, HasCallStack)

import Evaluator (evaluateProgram)
import EvaluatorEnv (ReadWrite(..))
import FuzzyParser (parseProgram)
import Solver (solveProgram)


-- -----------------
-- * Auxiliary

-- | A testable inner monad for the evaluator, which starts with a list of inputs and stores the generated output.
type IOStore = State ([String], String)

runIOStore :: IOStore a -> ([String], String) -> (a, ([String], String))
runIOStore = runState

-- | The IOStore monad is an instance of 'ReadWrite', since it can read from the stored inputs and write to the output storage.
instance ReadWrite IOStore where
    read = do
        input <- gets $ head . fst
        modify $ first tail
        return input
    write output = modify $ second (++output)

-- | An imitation of the main function which makes assertions instead of failing.
testExample ::
    FilePath -- ^ The path of the example to run.
    -> [String] -- ^ The inputs the program has to consume.
    -> String -- ^ The expected outputs of the program.
    -> IO ()
testExample fileName inputs expOutputs = do
    readRes <- tryIOError $ readFile fileName
    fc <- case readRes of
        Left _ -> assertFailure "The input file could not be opened"
        Right str -> return str

    unsolvedProg <- case parseProgram fc of
        Left err -> assertFailure $ "Example failed parsing, the error was:\n" ++ show err
        Right prog -> return prog

    solvedProg <- case solveProgram unsolvedProg of
        (Left err, _) -> assertFailure $ "Example failed solving, the error was:\n" ++ show err
        (Right ((prog, _), _), _) -> return prog

    let (res, (uncInputs, outputs)) = runIOStore (evaluateProgram solvedProg) (inputs, [])
    case res of
        Left err -> assertFailure $ "Example failed evaluating, the error was:\n" ++ show err
        Right _ ->
            if null uncInputs
                then outputs @?= expOutputs
                else assertFailure $ "Example didn't consume all input:\n" ++ show uncInputs

-- | A test case for an example with a list of inputs and the expected output.
exampleTestCase ::
    String -- ^ The name of the program to run.
    -> [String] -- ^ The inputs the program has to consume.
    -> String -- ^ The expected outputs of the program.
    -> TestTree
exampleTestCase progName inputs expOutputs = testCase progName $ testExample ("examples/" ++ progName ++ ".epl") inputs expOutputs


-- -----------------
-- * Tests

exampleTests :: TestTree
exampleTests = testGroup "example"
    [
        exampleTestCase "Pow" [] "2 to the power of 3 is 8",

        exampleTestCase "RecFib" [] "The 20th fibonacci number is 6765",

        exampleTestCase "SeqFib" [] "The 20th fibonacci number is 6765",

        exampleTestCase "Sum" [] "The sum of [1, 2, 3.5] is 6.5",

        exampleTestCase "Lists" [] "The shuffled list is 3142\nThe first half of the list is 31\n5 -> 9\n6 -> 9\n7 -> 9\n8 -> 9\n",

        exampleTestCase "Flatten" [] "[[1, 2, 3], [4, 5, 6], [7, 8, 9]] flattened is [1, 2, 3, 4, 5, 6, 7, 8, 9]",

        exampleTestCase "Iterators" [] "[1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5]",

        exampleTestCase "Head" [] "The list is empty\nThe head of the list is 1"
    ]

tests :: TestTree
tests = testGroup "Evaluator"
    [
        exampleTests
    ]
