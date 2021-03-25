module SolverTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import TestUtils
import qualified PreludeDefs as D
import qualified ParserEnv as Env
import qualified Solver as S
import qualified AST as T

--


-- Tests

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (S.getValueType $ T.OperatorCall "%_plus_%" [T.IntV 2, T.IntV 3])
                envWithFunctions
                T.IntT,

        testCase "Adding mixed" $
            expectedResult
                (S.getValueType $ T.OperatorCall "%_plus_%" [T.IntV 2, T.FloatV 3.0])
                envWithFunctions
                T.FloatT
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Valid mismatching types" $
            expectedSuccess
                (Env.setVariableType ["a"] T.IntT >> S.setVariableTypeWithCheck ["a"] T.FloatT)
                emptyEnv,

        testCase "Invalid mismatching types" $
            expectedFailure
                (Env.setVariableType ["a"] T.IntT >> S.setVariableTypeWithCheck ["a"] T.BoolT)
                emptyEnv,

        testCase "Repeated new variable" $
            expectedFailure
                (Env.setVariableType ["a"] T.IntT >> S.setNewVariableType ["a"] T.IntT)
                emptyEnv
    ]

checkValueIntegrityTests :: TestTree
checkValueIntegrityTests = testGroup "Check value integrity"
    [
        testCase "Correct type arguments" $
            expectedSuccess
                (S.checkValueIntegrity $ T.OperatorCall "%_plus_%" [T.IntV 2, T.FloatV 3.0])
                envWithFunctions,

        testCase "Wrong type arguments" $
            expectedFailure
                (S.checkValueIntegrity $ T.OperatorCall "%_plus_%" [T.BoolV True, T.BoolV False])
                envWithFunctions
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (S.solveValue (T.ValueM [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]))
                envWithFunctions
                (T.OperatorCall "%_plus_%" [T.OperatorCall "%_times_%" [T.IntV 2, T.IntV 3], T.OperatorCall "%_times_%" [T.IntV 4, T.IntV 5]]),

        testCase "List" $
            expectedResult
                (S.solveValue (T.ListV T.IntT [T.ValueM [T.IntP 2, T.WordP "times", T.IntP 3], T.ValueM [T.IntP 4, T.WordP "times", T.IntP 5]]))
                envWithFunctions
                (T.ListV T.IntT [T.OperatorCall "%_times_%" [T.IntV 2, T.IntV 3], T.OperatorCall "%_times_%" [T.IntV 4, T.IntV 5]]),

        testCase "List with wrong items" $
            expectedFailure
                (S.solveValue (T.ListV T.IntT [T.BoolV True, T.BoolV False]))
                emptyEnv

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
