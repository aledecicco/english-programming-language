{-# LANGUAGE FlexibleInstances #-}

module ExamplesTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )
import Control.Monad.Trans.State ( State, modify, gets, runState )

import FuzzyParser ( parseProgram )
import Solver ( solveProgram )
import Evaluator (evaluateProgram)
import EvaluatorEnv (ReadWrite(..))

--


--Auxiliary

type IOStore = State ([String], String)

runIOStore :: IOStore a -> ([String], String) -> (a, ([String], String))
runIOStore = runState

instance ReadWrite IOStore where
    read = do
        s <- gets $ head . fst
        modify $ \(i, o) -> (tail i, o)
        return s
    write s = modify $ \(i, o) -> (i, o ++ s)

testExample :: FilePath -> [String] -> String -> IO ()
testExample fn i o = do
    fc <- readFile fn
    case parseProgram fc of
        Left e -> assertFailure $ "Example failed parsing, the error was:\n" ++ show e
        Right p ->
            case solveProgram p of
                (Left e', _) -> assertFailure $ "Example failed solving, the error was:\n" ++ show e'
                (Right ((p', _), _), _) -> do
                    let (r, (i', o')) = runIOStore (evaluateProgram p') (i, [])
                    case r of
                        Left e'' -> assertFailure $ "Example failed evaluating, the error was:\n" ++ show e''
                        Right f ->
                            if null i'
                                then o' @?= o
                                else assertFailure $ "Example didn't consume all input:\n" ++ show i'

exampleTestCase :: String -> [String] -> String -> TestTree
exampleTestCase en i o = testCase en $ testExample ("examples/" ++ en ++ ".epl") i o

--


-- Tests

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

--


-- Main

tests :: TestTree
tests = testGroup "Evaluator"
    [
        exampleTests
    ]

--
