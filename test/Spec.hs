import Test.Tasty ( defaultMainWithIngredients, testGroup, TestTree )
import Test.Tasty.Ingredients.Basic ( listingTests, consoleTestReporter )
import Test.Tasty.Ingredients.Rerun ( rerunningTests )

import qualified FuzzyParserTest
import qualified SolverTest
import qualified EvaluatorTest
import qualified ExamplesTest

--


--

main :: IO ()
main =
    defaultMainWithIngredients
        [
            rerunningTests [listingTests, consoleTestReporter]
        ]
        tests

tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests, SolverTest.tests, EvaluatorTest.tests, ExamplesTest.tests]

--
