import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified ParserTest
import qualified MatcherTest

--


--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests, MatcherTest.tests]
