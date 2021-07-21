module SolverTest ( tests ) where

import Control.Monad ( void )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import BuiltInDefs ( builtInOperators, builtInProcedures )
import Errors
import SolverEnv
import Solver
import AST

--


-- Auxiliary

stateWithFunctions :: SolverData
stateWithFunctions =
    let (_, vs) = initialState
    in (builtInOperators ++ builtInProcedures, vs)

-- Asserts that a parser action yields a specific result with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => SolverEnv a -> SolverData -> a -> Assertion
expectedResult a s r =
    case runSolverEnv a s initialLocation of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ show e
        Right ((r', _), _) -> r' @?= r

-- Asserts that a parser action succeeds with the given environment
expectedSuccess :: (HasCallStack, Show a) => SolverEnv a -> SolverData -> Assertion
expectedSuccess a s =
    case runSolverEnv a s initialLocation of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ show e
        Right _ -> return ()

-- Asserts that a parser action yields a specific error with the given environment
expectedError :: (HasCallStack, Show a) => SolverEnv a -> SolverData -> Error -> Assertion
expectedError a s e =
    case runSolverEnv a s initialLocation of
        Left e' -> e' @?= e
        Right ((r, _), _) -> assertFailure $ "Parser action didn't fail, the result was " ++ show r

--


-- Tests

splitsTests :: TestTree
splitsTests = testGroup "Splits"
    [
        testCase "Addition" $
            splits [IntP () 2, WordP () "plus", IntP () 3]
            @?=
            [
                ([IntP () 2], [WordP () "plus", IntP () 3]),
                ([IntP () 2, WordP () "plus"], [IntP () 3]),
                ([IntP () 2, WordP () "plus", IntP () 3], [])
            ],

        testCase "Addition with recursive arguments" $
            splits [IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]
            @?=
            [
                ([IntP () 2], [WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]),
                ([IntP () 2, WordP () "times"], [IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]),
                ([IntP () 2, WordP () "times", IntP () 3], [WordP () "plus", IntP () 4, WordP () "times", IntP () 5]),
                ([IntP () 2, WordP () "times", IntP () 3, WordP () "plus"], [IntP () 4, WordP () "times", IntP () 5]),
                ([IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4], [WordP () "times", IntP () 5]),
                ([IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times"], [IntP () 5]),
                ([IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5], [])
            ]
    ]

sepByTitleTests :: TestTree
sepByTitleTests = testGroup "Sep by title"
    [
        testCase "Addition" $
            sepByTitle
                [IntP () 2, WordP () "plus", IntP () 3]
                [TitleParam () ["m"] FloatT, TitleWords () ["plus"], TitleParam () ["n"] FloatT]
            @?=
            [[[IntP () 2], [IntP () 3]]],

        testCase "Addition with recursive arguments" $
            sepByTitle
                [IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]
                [TitleParam () ["m"] FloatT, TitleWords () ["plus"], TitleParam () ["n"] FloatT]
            @?=
            [[[IntP () 2, WordP () "times", IntP () 3],[IntP () 4, WordP () "times", IntP () 5]]],

        testCase "Multiplication with recursive arguments" $
            sepByTitle
                [IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]
                [TitleParam () ["m"] FloatT, TitleWords () ["times"], TitleParam () ["n"] FloatT]
            @?=
            [
                [[IntP () 2], [IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]],
                [[IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4], [IntP () 5]]
            ]
    ]

asNameTests :: TestTree
asNameTests = testGroup "As name"
    [
        testCase "Many words" $
            expectedResult
                (matchAsName [WordP () "short", WordP () "name"])
                initialState
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedResult
                (matchAsName [WordP () "short", WordP () "name", IntP () 1])
                initialState
                Nothing
    ]

asFunctionCallTests :: TestTree
asFunctionCallTests = testGroup "As function call"
    [
        testCase "Arguments at beggining and end" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3] $ map snd builtInOperators)
                stateWithFunctions
                [("%_plus_%", [IntV (0,0) 2, IntV (0,7) 3])],

        testCase "Addition and multiplication associativity" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5] $ map snd builtInOperators)
                stateWithFunctions
                [
                    ("%_plus_%", [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]]),
                    ("%_times_%", [IntV (0,0) 2, OperatorCall (0,8) "%_plus_%" [IntV (0,8) 3, OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]]]),
                    ("%_times_%", [IntV (0,0) 2, OperatorCall (0,8) "%_times_%" [OperatorCall (0,8) "%_plus_%" [IntV (0,8) 3, IntV (0,15) 4], IntV (0,23) 5]]),
                    ("%_times_%", [OperatorCall (0,0) "%_plus_%" [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], IntV (0,15) 4], IntV (0,23) 5]),
                    ("%_times_%", [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, OperatorCall (0,8) "%_plus_%" [IntV (0,8) 3, IntV (0,15) 4]], IntV (0,23) 5])
                ],

        testCase "Forced associativity with parenthesis" $
            expectedResult
                (matchAsFunctionCall [ParensP [IntP (0,1) 2, WordP (0,3) "times", IntP (0,9) 3, WordP (0,11) "plus", IntP (0,16) 4], WordP (0,19) "times", IntP (0,25) 5] $ map snd builtInOperators)
                stateWithFunctions
                [
                    ("%_times_%", [OperatorCall (0,1) "%_plus_%" [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], IntV (0,16) 4], IntV (0,25) 5]),
                    ("%_times_%", [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, OperatorCall (0,9) "%_plus_%" [IntV (0,9) 3, IntV (0,16) 4]], IntV (0,25) 5])
                ],

        testCase "Wrong type arguments" $
            expectedResult
                (matchAsFunctionCall [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"] $ map snd builtInOperators)
                stateWithFunctions
                [("%_plus_%", [BoolV (0,0) True, BoolV (0,10) False])],

        testCase "Wrong functions category" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3] $ map snd builtInProcedures)
                stateWithFunctions
                []
    ]

asOperatorCallTests :: TestTree
asOperatorCallTests = testGroup "As operator call"
    [
        testCase "Procedure matchable" $
            expectedResult
                (matchAsOperatorCall [WordP (0,0) "print", IntP (0,6) 3])
                stateWithFunctions
                []
    ]


asProcedureCallTests :: TestTree
asProcedureCallTests = testGroup "As procedure call"
    [
        testCase "Operator matchable" $
            expectedResult
                (matchAsProcedureCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3])
                stateWithFunctions
                []
    ]

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, IntV (0,0) 3])
                stateWithFunctions
                IntT,

        testCase "Adding mixed" $
            expectedResult
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, FloatV (0,0) 3.0])
                stateWithFunctions
                FloatT,

        testCase "Correct type arguments" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, FloatV (0,6) 3.0])
                stateWithFunctions,

        testCase "Correct bound types" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) IntT [IntV (0,1) 1, IntV (0,3) 2], ListV (0,17) IntT [IntV (0,18) 3, IntV (0,20) 4]])
                stateWithFunctions,

        testCase "Satisfiable bound types" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) FloatT [FloatV (0,1) 1, IntV (0,3) 2], ListV (0,17) IntT [IntV (0,18) 3, IntV (0,20) 4]])
                stateWithFunctions,

        testCase "Wrong bound types" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) BoolT [BoolV (0,1) True, BoolV (0,7) False], ListV (0,21) IntT [IntV (0,22) 3, IntV (0,24) 4]])
                stateWithFunctions
                (Error (Just (0,21)) $ WrongTypeParameter (ListT BoolT) (ListT IntT) ["n"]),

        testCase "Wrong type arguments" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 1, BoolV (0,3) True])
                stateWithFunctions
                (Error (Just (0,3)) $ WrongTypeParameter FloatT BoolT ["n"]),

        testCase "Wrong type lists" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) BoolT [BoolV (0,1) True, BoolV (0,6) False], ListV (0,20) IntT [IntV (0,21) 3, IntV (0,23) 4]])
                stateWithFunctions
                (Error (Just (0,20)) $ WrongTypeParameter (ListT BoolT) (ListT IntT) ["n"])
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Valid mismatching types" $
            expectedSuccess
                (setVariableType ["a"] FloatT >> setVariableTypeWithCheck ["a"] IntT)
                initialState,

        testCase "Invalid mismatching types" $
            expectedError
                (setVariableType ["a"] IntT >> setVariableTypeWithCheck ["a"] BoolT)
                initialState
                (Error (Just (0,0)) $ MismatchingTypeAssigned IntT BoolT ["a"]),

        testCase "Repeated new variable" $
            expectedError
                (setVariableType ["a"] IntT >> setNewVariableType ["a"] IntT)
                initialState
                (Error (Just (0,0)) $ VariableAlreadyDefined ["a"])
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (solveValueWithType IntT $ ValueM (0,0) [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5])
                stateWithFunctions
                (OperatorCall (0,0) "%_plus_%" [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]]),

        testCase "List" $
            expectedResult
                (solveValueWithType (ListT IntT) $ ListV (0,0) IntT [ValueM (0,1) [IntP (0,1) 2, WordP (0,3) "times", IntP (0,9) 3], ValueM (0,11) [IntP (0,11) 4, WordP (0,13) "times", IntP (0,19) 5]])
                stateWithFunctions
                (ListV (0,0) IntT [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], OperatorCall (0,11) "%_times_%" [IntV (0,11) 4, IntV (0,19) 5]]),

        testCase "Matchable with wrong type arguments" $
            expectedError
                (solveValueWithType IntT $ ValueM (0,0) [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"])
                stateWithFunctions
                (Error (Just (0,0)) (WrongTypeParameter FloatT BoolT ["m"])),

        testCase "Ambiguous matchable with wrong type arguments" $
            expectedError
                (solveValueWithType IntT $ ValueM (0,0) [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false", WordP (0,16) "plus", WordP (0,21) "true"])
                stateWithFunctions
                (Error (Just (0,0)) (UnmatchableValueTypes [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false", WordP (0,16) "plus", WordP (0,21) "true"])),

        testCase "List with wrong items" $
            expectedError
                (solveValueWithType (ListT IntT) $ ListV (0,0) IntT [BoolV (0,1) True, BoolV (0,7) False])
                initialState
                (Error (Just (0,1)) $ WrongTypeValue IntT BoolT)
    ]

--


-- Main

tests :: TestTree
tests = testGroup "Solver"
    [
        splitsTests,
        sepByTitleTests,
        asNameTests,
        asFunctionCallTests,
        asOperatorCallTests,
        asProcedureCallTests,
        getValueTypeTests,
        setVariableTypeTests,
        solveValueTests
    ]

--
