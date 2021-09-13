{-
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The entry point of the interpreter's test suite.
-}

import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified FuzzyParserTest
import qualified SolverTest
import qualified EvaluatorTest
import qualified ExamplesTest


tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests, SolverTest.tests, EvaluatorTest.tests, ExamplesTest.tests]

main :: IO ()
main = defaultMain tests
