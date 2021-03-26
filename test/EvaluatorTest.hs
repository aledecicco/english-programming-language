module EvaluatorTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import PreludeDefs
import EvaluatorEnv
import Evaluator
import AST

--


-- Assertions

-- Asserts that an evaluator action yields a specific result with the given environment
expectedResult :: HasCallStack => EvaluatorEnv (Maybe Value) -> EvaluatorState -> Value -> Assertion
expectedResult eval st res = do
    r <- runEvaluatorEnv eval st
    case r of
        Left err -> assertFailure $ "Evaluator action failed, the error was:\n" ++ err
        Right (Nothing, _) -> assertFailure "Evaluator action didn't yield a result"
        Right (Just res', _) -> res' @?= res

-- Asserts that an evaluator action succeeds with the given environment
expectedSuccess :: HasCallStack => EvaluatorEnv (Maybe Value) -> EvaluatorState -> Assertion
expectedSuccess eval st = do
    r <- runEvaluatorEnv eval st
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that an evaluator action fails with the given environment
expectedFailure :: HasCallStack => EvaluatorEnv (Maybe Value) -> EvaluatorState -> Assertion
expectedFailure eval st = do
    r <- runEvaluatorEnv eval st
    case r of
        Left _ -> return ()
        Right r -> assertFailure $ "Evaluator action didn't fail, the result was " ++ show r

--


-- Tests

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "Operator call" $
            expectedResult
                (Just <$> evaluateValue (OperatorCall "%_plus_%" [IntV 2, IntV 3]))
                ([("%_plus_%", [])], [], 0)
                (IntV 5),

        testCase "Variable" $
            expectedResult
                (Just <$> evaluateValue (VarV ["x"]))
                ([], [(["x"], IntV 5)], 0)
                (IntV 5)

    ]

sentenceTests :: TestTree
sentenceTests = testGroup "sentence"
    [
        testCase "While" $
            expectedResult
                (
                    evaluateSentenceLines
                        [
                            Line 0 $ While
                                (OperatorCall "%_is_less_than_%" [VarV ["x"], IntV 3])
                                [Line 1 (VarDef [["x"]] (OperatorCall "%_plus_%" [VarV ["x"], IntV 1]))],
                            Line 3 $ Result (VarV ["x"])
                        ]
                )
                ([("%_is_less_than_%", []), ("%_plus_%", [])], [(["x"], IntV (-1))], 0)
                (IntV 3)
    ]

--


-- Main

tests :: TestTree
tests = testGroup "Evaluator"
    [
        valueTests,
        sentenceTests
    ]

--
