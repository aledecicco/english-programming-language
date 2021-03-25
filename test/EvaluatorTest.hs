module EvaluatorTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )

import TestUtils
import qualified PreludeDefs as D
import qualified EvaluatorEnv as Env
import qualified Evaluator as E
import qualified AST as T

--


-- Tests

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "Operator call" $ do
            x <- Env.runEvaluatorEnv (E.evaluateValue $ T.OperatorCall "%_plus_%" [T.IntV 2, T.IntV 3]) ([("%_plus_%", [])], [], 0)
            case x of
                Right (r, _) -> r @?= T.IntV 5
                Left e -> assertFailure e
    ]


--


-- Main

tests :: TestTree
tests = testGroup "Evaluator"
    [
        valueTests
    ]

--
