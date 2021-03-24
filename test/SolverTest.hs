module SolverTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import TestUtils
import qualified PreludeDefs as D
import qualified ParserEnv as E
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
        testCase "Repeated new variable" $
            expectedFailure
                (E.setVariableType ["a"] T.IntT >> S.setNewVariableType ["a"] T.IntT)
                emptyEnv,

        testCase "Invalid mismatching types" $
            expectedFailure
                (E.setVariableType ["a"] T.IntT >> S.setVariableTypeWithCheck ["a"] T.BoolT)
                emptyEnv,

        testCase "Valid mismatching types" $
            expectedSuccess
                (E.setVariableType ["a"] T.IntT >> S.setVariableTypeWithCheck ["a"] T.FloatT)
                emptyEnv
    ]


--


-- Main

tests :: TestTree
tests = testGroup "Solver"
    [
        getValueTypeTests,
        setVariableTypeTests
    ]

--
