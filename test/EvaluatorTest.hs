{-|
Module      : EvaluatorTest
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The "Evaluator"'s test suite.
-}

module EvaluatorTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), Assertion, HasCallStack)
import qualified Data.Map.Strict as M

import AST
import Errors
import Evaluator
import EvaluatorEnv


-- -----------------
-- * Assertions

-- | Asserts that a computation yields a specific value as a result.
expectedResult :: HasCallStack => EvaluatorEnv IO (Maybe EvalRes) -> EvalRes -> Assertion
expectedResult action expVal = do
    res <- runEvaluatorEnv action initialLocation initialState
    case res of
        Left err -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show err
        Right ((Nothing , _), _) -> assertFailure "Evaluator action didn't yield a result"
        Right ((Just val, _), _) -> val @?= expVal

-- | Asserts that a computation succeeds.
expectedSuccess :: HasCallStack => EvaluatorEnv IO (Maybe EvalRes) -> Assertion
expectedSuccess action = do
    res <- runEvaluatorEnv action initialLocation initialState
    case res of
        Left err -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show err
        Right _ -> return ()

-- | Asserts that an action yields a specific error.
expectedError :: HasCallStack => EvaluatorEnv IO (Maybe EvalRes) -> Error -> Assertion
expectedError action expErr = do
    res <- runEvaluatorEnv action initialLocation initialState
    case res of
        Left err -> err @?= expErr
        Right ((Nothing, _), _) -> assertFailure "Evaluator action didn't fail, and yielded no result"
        Right ((Just val, _), _) -> assertFailure $ "Evaluator action didn't fail, the result was:\n" ++ show val


-- -----------------
-- * Tests

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "Operator call" $
            expectedResult
                (Just . ValRes <$> evaluateValue (OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, IntV (0,6) 3]))
                (ValRes $ IntV () 5),

        testCase "Variable" $
            expectedResult
                (setVariableValue ["x"] (IntV () 5) >> Just . ValRes <$> evaluateValue (VarV (0,0) ["x"]))
                (ValRes $ IntV () 5)

    ]

sentenceTests :: TestTree
sentenceTests = testGroup "sentence"
    [
        testCase "While" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 0),
                            While ()
                                (OperatorCall () "%_is_less_than_%" [VarV () ["x"], IntV () 3])
                                [ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]]],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 3),

        testCase "Add to" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            ProcedureCall () "add_%_to_%" [IntV () 3, VarV () ["x"]],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 5),

        testCase "Divide by" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 2],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ FloatV () 1.0),

        testCase "Append to" $
            expectedResult
                (do
                    res <- evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just $ ListT FloatT) (ListV () FloatT [FloatV () 5.0, FloatV () 4.0]),
                            ProcedureCall () "append_%_to_%" [ListV () IntT [IntV () 3, IntV () 2, IntV () 1], VarV () ["x"]],
                            Return () (VarV () ["x"])
                        ]
                    case res of
                        Just (ValRes val) -> Just . ValRes <$> loadReferences val
                        Just _ -> return res
                        Nothing -> return Nothing
                )
                (ValRes $ ListV () FloatT [FloatV () 5.0, FloatV () 4.0, IntV () 3, IntV () 2, IntV () 1]),

        testCase "Caught division by zero" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            Attempt () [ProcedureCall () "divide_%_by_%" [VarV () ["x"], FloatV () 0.0]],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 2),

        testCase "Variable definition before caught throw" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 2),
                            TryCatch ()
                                [ProcedureCall () "divide_%_by_%" [VarV () ["x"], FloatV () 0.0]]
                                [Return () (VarV () ["x"])]
                        ]
                )
                (ValRes $ IntV () 2),

        testCase "Nested tries and catches with rollback" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 6),
                            Attempt ()
                                [
                                    ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]],
                                    TryCatch ()
                                        [
                                            ProcedureCall () "multiply_%_by_%" [VarV () ["x"], IntV () 2],
                                            ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 0]
                                        ]
                                        [ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]]],
                                    ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]]
                                ],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 9),

        testCase "Break before error" $
            expectedResult
                (
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 1),
                            While () (BoolV () True)
                                [
                                    ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]],
                                    Break (),
                                    ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 0]
                                ],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 2),

        testCase "Exit before error" $
            expectedResult
                (do
                    setFunctionCallable
                        "test_%"
                        (FunCallable
                            [TitleWords () ["test"], TitleParam () [["x"]] (RefT IntT)]
                            (map mockLocations [
                                ProcedureCall () "add_%_to_%" [IntV () 1, VarV () ["x"]],
                                Exit (),
                                ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 0]
                            ])
                        )
                    evaluateSentences $
                        map mockLocations [
                            VarDef () [["x"]] (Just IntT) (IntV () 1),
                            ProcedureCall ()
                                "test_%"
                                [VarV () ["x"]],
                            Return () (VarV () ["x"])
                        ]
                )
                (ValRes $ IntV () 2),

        testCase "Caught throw" $
            expectedSuccess
                (evaluateSentences $ map mockLocations [Attempt () [Throw () ["test", "error"]]]),

        testCase "Garbage collector" $
            expectedSuccess
                (do
                    setFunctionCallable
                        "test_%"
                        (FunCallable
                            [TitleWords () ["test"], TitleParam () [["p"]] CharT ]
                            [mockLocations $ VarDef () [["M"]] Nothing (ListV () CharT [CharV () '1', CharV () '2', CharV () '3'])]
                        )
                    evaluateSentence $ mockLocations
                        (ProcedureCall ()
                            "test_%"
                            [IterV () (Just CharT) (ListV () CharT [CharV () 'a', CharV () 'b', CharV () 'c'])]
                        )
                ),

        testCase "Uncaught throw" $
            expectedError
                (evaluateSentences [Throw (0,0) ["test", "error"]])
                (Error (Just (0,0)) $ CodeError ["test", "error"]),

        testCase "Undefined variable after try" $
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

        testCase "Variable not in scope after true when" $
            expectedError
                (
                    evaluateSentences
                        [
                            When (0,0)
                                (BoolV (0,3) True)
                                [VarDef (1,0) [["x"]] (Just IntT) (IntV (1,6) 3)],
                            Return (2,0) (VarV (2,14) ["x"])
                        ]
                )
                (Error (Just (2,14)) $ UndefinedVariable ["x"]),

        testCase "Variable not in scope after false when" $
            expectedError
                (
                    evaluateSentences
                        [
                            When (0,0)
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
        mockLocations :: Functor a => Bare a -> Annotated a
        mockLocations = fmap $ const (0,0)

tests :: TestTree
tests = testGroup "Evaluator"
    [
        valueTests,
        sentenceTests
    ]
