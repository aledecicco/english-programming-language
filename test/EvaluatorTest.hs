module EvaluatorTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import BuiltInDefs ( builtInProcedures, builtInOperators )
import Errors
import EvaluatorEnv
import Evaluator
import AST

--


-- Auxiliary

stateWithFunctions :: EvaluatorData
stateWithFunctions =
    let (_, rs, vs, p) = initialState
    in (translateFunctions [] (builtInOperators ++ builtInProcedures), rs, vs, p)

-- Asserts that an evaluator action yields a specific result with the given environment
expectedResult :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Bare Value -> Assertion
expectedResult eval st res = do
    r <- runEvaluatorEnv eval st initialLocation
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show e
        Right ((Nothing , _), _) -> assertFailure "Evaluator action didn't yield a result"
        Right ((Just res', _), _) -> res' @?= res

-- Asserts that an evaluator action succeeds with the given environment
expectedSuccess :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Assertion
expectedSuccess eval st = do
    r <- runEvaluatorEnv eval st initialLocation
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ show e
        Right _ -> return ()

-- Asserts that an evaluator yields a specific error with the given environment
expectedError :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Error -> Assertion
expectedError eval st e = do
    r <- runEvaluatorEnv eval st initialLocation
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
               stateWithFunctions
                (IntV () 5),

        testCase "Variable" $
            expectedResult
                (setVariableValue ["x"] (IntV () 5) >> Just <$> evaluateValue (VarV (0,0) ["x"]))
                stateWithFunctions
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
                            VarDef () [["x"]] (IntV () 0),
                            While ()
                                (OperatorCall () "%_is_less_than_%" [VarV () ["x"], IntV () 3])
                                [VarDef () [["x"]] (OperatorCall () "%_plus_%" [VarV () ["x"], IntV () 1])],
                            Result () (VarV () ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 3),

        testCase "Variable in scope after if" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            If ()
                                (BoolV () True)
                                [VarDef () [["x"]] (IntV () 3)],
                            Result () (VarV () ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 3),

        testCase "Add to" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (IntV () 2),
                            ProcedureCall () "add_%_to_%" [IntV () 3, VarV () ["x"]],
                            Result () (VarV () ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 5),

        testCase "Divide by" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (IntV () 2),
                            ProcedureCall () "divide_%_by_%" [VarV () ["x"], IntV () 2],
                            Result () (VarV () ["x"])
                        ]
                )
                stateWithFunctions
                (FloatV () 1.0),

        testCase "Append to" $
            expectedResult
                (
                    evaluateSentences $
                        mockLocations [
                            VarDef () [["x"]] (ListV () FloatT [FloatV () 5.0, FloatV () 4.0]),
                            ProcedureCall () "append_%_to_%" [ListV () IntT [IntV () 3, IntV () 2, IntV () 1], VarV () ["x"]],
                            Result () (VarV () ["x"])
                        ]
                )
                stateWithFunctions
                (ListV () FloatT [FloatV () 5.0, FloatV () 4.0, IntV () 3, IntV () 2, IntV () 1]),

        testCase "Variable not in scope after if" $
            expectedError
                (
                    evaluateSentences
                        [
                            If (0,0)
                                (BoolV (0,3) False)
                                [VarDef (1,0) [["x"]] (IntV (1,6) 3)],
                            Result (2,0) (VarV (2,14) ["x"])
                        ]
                )
                stateWithFunctions
                (Error (Just (2,14)) $ UndefinedVariable ["x"]),

        testCase "Division by zero" $
            expectedError
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (IntV (0,6) 2),
                            ProcedureCall (1,0) "divide_%_by_%" [VarV (1,7) ["x"], FloatV (1,11) 0.0],
                            Result (2,0) (VarV (0,14) ["x"])
                        ]
                )
                stateWithFunctions
                (Error (Just (1,0)) DivisionByZero)
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
