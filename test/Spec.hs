import Test.Tasty
import qualified ParserTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ParserTest.tests]
