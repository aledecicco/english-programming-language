import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.Ingredients.Rerun ( defaultMainWithRerun )

import qualified FuzzyParserTest
import qualified SolverTest
import qualified EvaluatorTest

--


--

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests, SolverTest.tests, EvaluatorTest.tests]

--
