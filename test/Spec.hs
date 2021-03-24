import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified FuzzyParserTest
import qualified MatcherTest
import qualified SolverTest

--


--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests, MatcherTest.tests, SolverTest.tests]
