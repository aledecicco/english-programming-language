import Test.Tasty ( defaultMain, testGroup, TestTree )

import qualified ParserTest

--


--

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests]
