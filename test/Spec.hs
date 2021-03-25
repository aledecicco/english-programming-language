import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified FuzzyParserTest
import qualified MatcherTest
import qualified SolverTest
import qualified EvaluatorTest

--


--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests, MatcherTest.tests, SolverTest.tests, EvaluatorTest.tests]
