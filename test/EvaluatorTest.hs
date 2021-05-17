module EvaluatorTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import BuiltInDefs
import PrettyPrinter
import EvaluatorEnv
import Evaluator
import AST

--


-- Auxiliary

-- ToDo: find a cleaner way
stateWithFunctions :: EvaluatorData
stateWithFunctions =
    let (_, rs, vs, p) = initialState
    in (map convertFunction (builtInOperators ++ builtInProcedures), rs, vs, p)
    where
        convertFunction (fid, FunSignature t _) = (fid, FunCallable t [])

-- Asserts that an evaluator action yields a specific result with the given environment
expectedResult :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Bare Value -> Assertion
expectedResult eval st res = do
    r <- runEvaluatorEnv eval st initialLocation
    case r of
        Left err -> assertFailure $ "Evaluator action failed, the error was:\n" ++ err
        Right ((Nothing , _), _) -> assertFailure "Evaluator action didn't yield a result"
        Right ((Just res', _), _) -> res' @?= res

-- Asserts that an evaluator action succeeds with the given environment
expectedSuccess :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Assertion
expectedSuccess eval st = do
    r <- runEvaluatorEnv eval st initialLocation
    case r of
        Left e -> assertFailure $ "Evaluator action failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that an evaluator action fails with the given environment
expectedFailure :: HasCallStack => EvaluatorEnv (Maybe (Bare Value)) -> EvaluatorData -> Assertion
expectedFailure eval st = do
    r <- runEvaluatorEnv eval st initialLocation
    case r of
        Left _ -> return ()
        Right ((Nothing, _), _) -> assertFailure "Evaluator action didn't fail, and yielded no result"
        Right ((Just res, _), _) -> assertFailure $ "Evaluator action didn't fail, the result was " ++ ppValue res

--


-- Tests

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "Operator call" $
            expectedResult
                (Just <$> evaluateValue (OperatorCall () "%_plus_%" [IntV () 2, IntV () 3]))
               stateWithFunctions
                (IntV () 5),

        testCase "Variable" $
            expectedResult
                (setVariableValue ["x"] (IntV () 5) >> Just <$> evaluateValue (VarV () ["x"]))
                stateWithFunctions
                (IntV () 5)

    ]

-- ToDo: locations shouldn't matter
sentenceTests :: TestTree
sentenceTests = testGroup "sentence"
    [
        testCase "While" $
            expectedResult
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (IntV (0,0) 0),
                            While (0,0)
                                (OperatorCall (0,0) "%_is_less_than_%" [VarV (0,0) ["x"], IntV (0,0) 3])
                                [VarDef (0,0) [["x"]] (OperatorCall (0,0) "%_plus_%" [VarV (0,0) ["x"], IntV (0,0) 1])],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 3),

        testCase "Variable in scope after if" $
            expectedResult
                (
                    evaluateSentences
                        [
                            If (0,0)
                                (BoolV (0,0) True)
                                [VarDef (0,0) [["x"]] (IntV (0,0) 3)],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 3),

        testCase "Add to" $
            expectedResult
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (IntV (0,0) 2),
                            ProcedureCall (0,0) "add_%_to_%" [IntV (0,0) 3, VarV (0,0) ["x"]],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
                (IntV () 5),

        testCase "Divide by" $
            expectedResult
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (IntV (0,0) 2),
                            ProcedureCall (0,0) "divide_%_by_%" [VarV (0,0) ["x"], IntV (0,0) 2],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
                (FloatV () 1.0),

        testCase "Append to" $
            expectedResult
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (ListV (0,0) FloatT [FloatV (0,0) 5.0, FloatV (0,0) 4.0]),
                            ProcedureCall (0,0) "append_%_to_%" [ListV (0,0) IntT [IntV (0,0) 3, IntV (0,0) 2, IntV (0,0) 1], VarV (0,0) ["x"]],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
                (ListV () FloatT [FloatV () 5.0, FloatV () 4.0, IntV () 3, IntV () 2, IntV () 1]),

        testCase "Variable not in scope after if" $
            expectedFailure
                (
                    evaluateSentences
                        [
                            If (0,0)
                                (BoolV (0,0) False)
                                [VarDef (0,0) [["x"]] (IntV (0,0) 3)],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions,

        testCase "Division by zero" $
            expectedFailure
                (
                    evaluateSentences
                        [
                            VarDef (0,0) [["x"]] (IntV (0,0) 2),
                            ProcedureCall (0,0) "divide_%_by_%" [VarV (0,0) ["x"], FloatV (0,0) 0.0],
                            Result (0,0) (VarV (0,0) ["x"])
                        ]
                )
                stateWithFunctions
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
