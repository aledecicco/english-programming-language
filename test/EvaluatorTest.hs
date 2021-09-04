module EvaluatorTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )
import qualified Data.Map.Strict as M

import Errors
import EvaluatorEnv
import Evaluator
import AST

--


-- Auxiliary

-- Asserts that an evaluator action yields a specific result with the given environment
expectedResult :: HasCallStack => EvaluatorEnv IO (Maybe (Bare Value)) -> Bare Value -> Assertion
expectedResult eval res = do
    r <- runEvaluatorEnv eval initialLocation initialState
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show e
        Right ((Nothing , _), _) -> assertFailure "Evaluator action didn't yield a result"
        Right ((Just res', _), _) -> res' @?= res

-- Asserts that an evaluator action succeeds with the given environment
expectedSuccess :: HasCallStack => EvaluatorEnv IO (Maybe (Bare Value)) -> Assertion
expectedSuccess eval = do
    r <- runEvaluatorEnv eval initialLocation initialState
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show e
        Right _ -> return ()

-- Asserts that an evaluator yields a specific error with the given environment
expectedError :: HasCallStack => EvaluatorEnv IO (Maybe (Bare Value)) -> Error -> Assertion
expectedError eval e = do
    r <- runEvaluatorEnv eval initialLocation initialState
    case r of
        Left e' -> e' @?= e
        Right ((Nothing, _), _) -> assertFailure "Evaluator action didn't fail, and yielded no result"
        Right ((Just res, _), _) -> assertFailure $ "Evaluator action didn't fail, the result was " ++ show res

--


-- Tests

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "Operator call" $
            expectedResult
                (Just <$> evaluateValue (OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, IntV (0,6) 3]))
                (IntV () 5),

        testCase "Variable" $
            expectedResult
                (setVariableValue ["x"] (IntV () 5) >> Just <$> evaluateValue (VarV (0,0) ["x"]))
                (IntV () 5)

    ]

sentenceTests :: TestTree
sentenceTests = testGroup "sentence"
    [
        testCase "While" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 0),
                            While ()
                                (OperatorCall () "%_is_less_than_%" [VarV () ["x"], IntV () 3])
                                [ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]]],
                            Return () (VarV () ["x"])
                        ]
                )
                (IntV () 3),

        testCase "Add to" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            ProcedureCall () "add_%_to_%" [IntV () 3, VarV () ["x"]],
                            Return () (VarV () ["x"])
                        ]
                )
                (IntV () 5),

        testCase "Divide by" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 2],
                            Return () (VarV () ["x"])
                        ]
                )
                (FloatV () 1.0),

        testCase "Append to" $
            expectedResult
                (do
                    r <- evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just $ ListT FloatT) (ListV () FloatT [FloatV () 5.0, FloatV () 4.0]),
                            ProcedureCall () "append_%_to_%" [ListV () IntT [IntV () 3, IntV () 2, IntV () 1], VarV () ["x"]],
                            Return () (VarV () ["x"])
                        ]
                    case r of
                        Just v -> Just <$> loadReferences v
                        Nothing -> return Nothing
                )
                (ListV () FloatT [FloatV () 5.0, FloatV () 4.0, IntV () 3, IntV () 2, IntV () 1]),

        testCase "Caught division by zero" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            Try () [ProcedureCall () "divide_%_by_%" [VarV () ["x"], FloatV () 0.0]],
                            Return () (VarV () ["x"])
                        ]
                )
                (IntV () 2),

        testCase "Variable definition before caught throw" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            TryCatch ()
                                [ProcedureCall () "divide_%_by_%" [VarV () ["x"], FloatV () 0.0]]
                                [Return () (VarV () ["x"])]
                        ]
                )
                (IntV () 2),

        testCase "Caught throw" $
            expectedSuccess
                (evaluateSentences $ mockLocations [Try () [Throw () ["test", "error"]]]),

        testCase "Uncaught throw" $
            expectedError
                (evaluateSentences [Throw (0,0) ["test", "error"]])
                (Error (Just (0,0)) $ CodeError ["test", "error"]),

        testCase "Undefined variable after let with throw" $
            expectedError
                (
                    evaluateSentences
                        [
                            TryCatch (0,0)
                                [VarDef (1,4) [["x"]] Nothing (OperatorCall (1,13) "%_divided_by_%" [FloatV (1,13) 2.0, FloatV (1,28) 0.0])]
                                [Return (3,4) (VarV (3,18) ["x"])]
                        ]
                )
                (Error (Just (3,18)) $ UndefinedVariable ["x"]),

        testCase "Variable not in scope after true if" $
            expectedError
                (
                    evaluateSentences
                        [
                            If (0,0)
                                (BoolV (0,3) True)
                                [VarDef (1,0) [["x"]] (Just IntT) (IntV (1,6) 3)],
                            Return (2,0) (VarV (2,14) ["x"])
                        ]
                )
                (Error (Just (2,14)) $ UndefinedVariable ["x"]),

        testCase "Variable not in scope after false if" $
            expectedError
                (
                    evaluateSentences
                        [
                            If (0,0)
                                (BoolV (0,3) False)
                                [VarDef (1,0) [["x"]] (Just IntT) (IntV (1,6) 3)],
                            Return (2,0) (VarV (2,14) ["x"])
                        ]
                )
                (Error (Just (2,14)) $ UndefinedVariable ["x"]),

        testCase "Division by zero" $
            expectedError
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (Just IntT) (IntV (0,6) 2),
                            ProcedureCall (1,0) "divide_%_by_%" [VarV (1,7) ["x"], FloatV (1,11) 0.0],
                            Return (2,0) (VarV (0,14) ["x"])
                        ]
                )
                (Error (Just (1,0)) (CodeError ["Division by zero"]))
    ]
    where
        mockLocations :: Functor a => [a ()] -> [Annotated a]
        mockLocations = map . fmap $ const (0,0)

--


-- Main

tests :: TestTree
tests = testGroup "Evaluator"
    [
        valueTests,
        sentenceTests
    ]

--
