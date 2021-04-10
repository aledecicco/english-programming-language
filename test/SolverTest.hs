module SolverTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import ParserTestUtils
import ParserEnv
import Solver
import AST

--


-- Tests

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (getValueType $ OperatorCall "%_plus_%" [IntV 2, IntV 3])
                envWithFunctions
                IntT,

        testCase "Adding mixed" $
            expectedResult
                (getValueType $ OperatorCall "%_plus_%" [IntV 2, FloatV 3.0])
                envWithFunctions
                FloatT
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Valid mismatching types" $
            expectedSuccess
                (setVariableType ["a"] IntT >> setVariableTypeWithCheck ["a"] FloatT)
                emptyEnv,

        testCase "Invalid mismatching types" $
            expectedFailure
                (setVariableType ["a"] IntT >> setVariableTypeWithCheck ["a"] BoolT)
                emptyEnv,

        testCase "Repeated new variable" $
            expectedFailure
                (setVariableType ["a"] IntT >> setNewVariableType ["a"] IntT)
                emptyEnv
    ]

checkValueIntegrityTests :: TestTree
checkValueIntegrityTests = testGroup "Check value integrity"
    [
        testCase "Correct type arguments" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_plus_%" [IntV 2, FloatV 3.0])
                envWithFunctions,

        testCase "Correct bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV IntT [IntV 1, IntV 2], ListV IntT [IntV 3, IntV 4]])
                envWithFunctions,

        testCase "Satisfiable bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV FloatT [FloatV 1, IntV 2], ListV IntT [IntV 3, IntV 4]])
                envWithFunctions,

        testCase "Wrong bound types" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV BoolT [BoolV True, BoolV False], ListV IntT [IntV 3, IntV 4]])
                envWithFunctions,

        testCase "Wrong type arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_plus_%" [BoolV True, BoolV False])
                envWithFunctions,

        testCase "Ill-formed arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV IntT [BoolV True, BoolV False], ListV IntT [IntV 3, IntV 4]])
                envWithFunctions
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (solveValue (ValueM [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]))
                envWithFunctions
                (OperatorCall "%_plus_%" [OperatorCall "%_times_%" [IntV 2, IntV 3], OperatorCall "%_times_%" [IntV 4, IntV 5]]),

        testCase "List" $
            expectedResult
                (solveValue (ListV IntT [ValueM [IntP 2, WordP "times", IntP 3], ValueM [IntP 4, WordP "times", IntP 5]]))
                envWithFunctions
                (ListV IntT [OperatorCall "%_times_%" [IntV 2, IntV 3], OperatorCall "%_times_%" [IntV 4, IntV 5]]),

        testCase "List with wrong items" $
            expectedFailure
                (solveValue (ListV IntT [BoolV True, BoolV False]))
                emptyEnv,

        testCase "Matchable with wrong type arguments" $
            expectedFailure
                (solveValue (ValueM [WordP "true", WordP "plus", WordP "false"]))
                envWithFunctions
    ]


--


-- Main

tests :: TestTree
tests = testGroup "Solver"
    [
        getValueTypeTests,
        setVariableTypeTests,
        checkValueIntegrityTests,
        solveValueTests
    ]

--
