import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified FuzzyParserTest
import qualified MatcherTest

--


--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [FuzzyParserTest.tests]--, MatcherTest.tests]
