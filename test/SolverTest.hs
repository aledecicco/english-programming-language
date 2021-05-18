module SolverTest ( tests ) where

import Control.Monad ( void )
import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import BuiltInDefs ( builtInOperators, builtInProcedures )
import ParserEnv
import Solver
import AST

--


-- Auxiliary

stateWithFunctions :: ParserData
stateWithFunctions =
    let (_, vs) = initialState
    in (builtInOperators ++ builtInProcedures, vs)

-- Asserts that a parser action yields a specific result with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => ParserEnv a -> ParserData -> a -> Assertion
expectedResult a s r =
    case runParserEnv a s initialLocation of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ e
        Right ((r', _), _) -> r' @?= r

-- Asserts that a parser action yields a specific result ignoring annotations with the given environment
expectedBareResult :: (HasCallStack, Eq (a ()), Show (a ()), Functor a) => ParserEnv (a b) -> ParserData -> a () -> Assertion
expectedBareResult a s r =
    case runParserEnv a s initialLocation of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ e
        Right ((r', _), _) -> void r' @?= r

-- Asserts that a parser action succeeds with the given environment
expectedSuccess :: (HasCallStack, Show a) => ParserEnv a -> ParserData -> Assertion
expectedSuccess a s =
    case runParserEnv a s initialLocation of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that a parser action fails with the given environment
expectedFailure :: (HasCallStack, Show a) => ParserEnv a -> ParserData -> Assertion
expectedFailure a s =
    case runParserEnv a s initialLocation of
        Left _ -> return ()
        Right (r, _) -> assertFailure $ "Parser action didn't fail, the result was " ++ show r

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
                (Just ("%_plus_%", [IntV (0,0) 2, IntV (0,7) 3])),

        testCase "Addition and multiplication associativity" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_plus_%", [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]])),

        testCase "Forced associativity with parenthesis" $
            expectedResult
                (matchAsFunctionCall [ParensP [IntP (0,1) 2, WordP (0,3) "times", IntP (0,9) 3, WordP (0,11) "plus", IntP (0,16) 4], WordP (0,19) "times", IntP (0,25) 5] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_times_%", [OperatorCall (0,1) "%_plus_%" [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], IntV (0,16) 4], IntV (0,25) 5])),

        testCase "Wrong type arguments" $
            expectedResult
                (matchAsFunctionCall [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_plus_%", [BoolV (0,0) True, BoolV (0,10) False])),

        testCase "Wrong functions category" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3] $ map snd builtInProcedures)
                stateWithFunctions
                Nothing
    ]

asOperatorCallTests :: TestTree
asOperatorCallTests = testGroup "As operator call"
    [
        testCase "Procedure matchable" $
            expectedResult
                (matchAsOperatorCall [WordP (0,0) "print", IntP (0,6) 3])
                stateWithFunctions
                Nothing
    ]


asProcedureCallTests :: TestTree
asProcedureCallTests = testGroup "As procedure call"
    [
        testCase "Operator matchable" $
            expectedResult
                (matchAsProcedureCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3])
                stateWithFunctions
                Nothing
    ]

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (getValueType $ OperatorCall () "%_plus_%" [IntV () 2, IntV () 3])
                stateWithFunctions
                IntT,

        testCase "Adding mixed" $
            expectedResult
                (getValueType $ OperatorCall () "%_plus_%" [IntV () 2, FloatV () 3.0])
                stateWithFunctions
                FloatT
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Valid mismatching types" $
            expectedSuccess
                (setVariableType ["a"] FloatT >> setVariableTypeWithCheck ["a"] IntT)
                initialState,

        testCase "Invalid mismatching types" $
            expectedFailure
                (setVariableType ["a"] IntT >> setVariableTypeWithCheck ["a"] BoolT)
                initialState,

        testCase "Repeated new variable" $
            expectedFailure
                (setVariableType ["a"] IntT >> setNewVariableType ["a"] IntT)
                initialState
    ]

-- ToDo: locations shouldn't matter
checkValueIntegrityTests :: TestTree
checkValueIntegrityTests = testGroup "Check value integrity"
    [
        testCase "Correct type arguments" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, FloatV (0,0) 3.0])
                stateWithFunctions,

        testCase "Correct bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) IntT [IntV (0,0) 1, IntV (0,0) 2], ListV (0,0) IntT [IntV (0,0) 3, IntV (0,0) 4]])
                stateWithFunctions,

        testCase "Satisfiable bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) FloatT [FloatV (0,0) 1, IntV (0,0) 2], ListV (0,0) IntT [IntV (0,0) 3, IntV (0,0) 4]])
                stateWithFunctions,

        testCase "Wrong bound types" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) BoolT [BoolV (0,0) True, BoolV (0,0) False], ListV (0,0) IntT [IntV (0,0) 3, IntV (0,0) 4]])
                stateWithFunctions,

        testCase "Wrong type arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall (0,0) "%_plus_%" [BoolV (0,0) True, BoolV (0,0) False])
                stateWithFunctions,

        testCase "Ill-formed arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) IntT [BoolV (0,0) True, BoolV (0,0) False], ListV (0,0) IntT [IntV (0,0) 3, IntV (0,0) 4]])
                stateWithFunctions
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (solveValue (ValueM (0,0) [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5]))
                stateWithFunctions
                (OperatorCall (0,0) "%_plus_%" [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]]),

        testCase "List" $
            expectedResult
                (solveValue (ListV (0,0) IntT [ValueM (0,1) [IntP (0,1) 2, WordP (0,3) "times", IntP (0,9) 3], ValueM (0,11) [IntP (0,11) 4, WordP (0,13) "times", IntP (0,19) 5]]))
                stateWithFunctions
                (ListV (0,0) IntT [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], OperatorCall (0,11) "%_times_%" [IntV (0,11) 4, IntV (0,19) 5]]),

        testCase "List with wrong items" $
            expectedFailure
                (solveValue (ListV (0,0) IntT [BoolV (0,1) True, BoolV (0,7) False]))
                initialState,

        testCase "Matchable with wrong type arguments" $
            expectedFailure
                (solveValue (ValueM (0,0) [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"]))
                stateWithFunctions
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
        checkValueIntegrityTests,
        solveValueTests
    ]

--
