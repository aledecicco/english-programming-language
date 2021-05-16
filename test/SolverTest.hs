module SolverTest ( tests ) where

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
            expectedBareResult
                (matchAsName [WordP (0,0) "short", WordP (0,0) "name"])
                initialState
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedBareResult
                (matchAsName [WordP (0,0) "short", WordP (0,0) "name", IntP () 1])
                initialState
                Nothing
    ]

asFunctionCallTests :: TestTree
asFunctionCallTests = testGroup "As function call"
    [
        testCase "Arguments at beggining and end" $
            expectedBareResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,0) "plus", IntP (0,0) 3] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_plus_%", [IntV () 2, IntV () 3])),

        testCase "Addition and multiplication associativity" $
            expectedBareResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,0) "times", IntP (0,0) 3, WordP (0,0) "plus", IntP (0,0) 4, WordP (0,0) "times", IntP (0,0) 5] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_plus_%", [OperatorCall () "%_times_%" [IntV () 2, IntV () 3], OperatorCall () "%_times_%" [IntV () 4, IntV () 5]])),

        testCase "Forced associativity with parenthesis" $
            expectedBareResult
                (matchAsFunctionCall [ParensP (0,0) [IntP (0,0) 2, WordP (0,0) "times", IntP (0,0) 3, WordP (0,0) "plus", IntP (0,0) 4], WordP (0,0) "times", IntP (0,0) 5] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_times_%", [OperatorCall () "%_plus_%" [OperatorCall () "%_times_%" [IntV () 2, IntV () 3], IntV () 4], IntV () 5])),

        testCase "Wrong type arguments" $
            expectedBareResult
                (matchAsFunctionCall [WordP (0,0) "true", WordP (0,0) "plus", WordP (0,0) "false"] $ map snd builtInOperators)
                stateWithFunctions
                (Just ("%_plus_%", [BoolV () True, BoolV () False])),

        testCase "Wrong functions category" $
            expectedBareResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,0) "plus", IntP (0,0) 3] $ map snd builtInProcedures)
                stateWithFunctions
                Nothing
    ]

asOperatorCallTests :: TestTree
asOperatorCallTests = testGroup "As operator call"
    [
        testCase "Procedure matchable" $
            expectedResult
                (matchAsOperatorCall [WordP (0,0) "print", IntP (0,0) 3])
                stateWithFunctions
                Nothing
    ]


asProcedureCallTests :: TestTree
asProcedureCallTests = testGroup "As procedure call"
    [
        testCase "Operator matchable" $
            expectedResult
                (matchAsProcedureCall [IntP (0,0) 2, WordP (0,0) "plus", IntP (0,0) 3])
                stateWithFunctions
                Nothing
    ]

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (getValueType $ OperatorCall "%_plus_%" [IntV 2, IntV 3])
                stateWithFunctions
                IntT,

        testCase "Adding mixed" $
            expectedResult
                (getValueType $ OperatorCall "%_plus_%" [IntV 2, FloatV 3.0])
                stateWithFunctions
                FloatT
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Valid mismatching types" $
            expectedSuccess
                (setVariableType ["a"] IntT >> setVariableTypeWithCheck ["a"] FloatT)
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

checkValueIntegrityTests :: TestTree
checkValueIntegrityTests = testGroup "Check value integrity"
    [
        testCase "Correct type arguments" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_plus_%" [IntV 2, FloatV 3.0])
                stateWithFunctions,

        testCase "Correct bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV IntT [IntV 1, IntV 2], ListV IntT [IntV 3, IntV 4]])
                stateWithFunctions,

        testCase "Satisfiable bound types" $
            expectedSuccess
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV FloatT [FloatV 1, IntV 2], ListV IntT [IntV 3, IntV 4]])
                stateWithFunctions,

        testCase "Wrong bound types" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV BoolT [BoolV True, BoolV False], ListV IntT [IntV 3, IntV 4]])
                stateWithFunctions,

        testCase "Wrong type arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_plus_%" [BoolV True, BoolV False])
                stateWithFunctions,

        testCase "Ill-formed arguments" $
            expectedFailure
                (checkValueIntegrity $ OperatorCall "%_appended_to_%" [ListV IntT [BoolV True, BoolV False], ListV IntT [IntV 3, IntV 4]])
                stateWithFunctions
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (solveValue (ValueM [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]))
                stateWithFunctions
                (OperatorCall "%_plus_%" [OperatorCall "%_times_%" [IntV 2, IntV 3], OperatorCall "%_times_%" [IntV 4, IntV 5]]),

        testCase "List" $
            expectedResult
                (solveValue (ListV IntT [ValueM [IntP 2, WordP "times", IntP 3], ValueM [IntP 4, WordP "times", IntP 5]]))
                stateWithFunctions
                (ListV IntT [OperatorCall "%_times_%" [IntV 2, IntV 3], OperatorCall "%_times_%" [IntV 4, IntV 5]]),

        testCase "List with wrong items" $
            expectedFailure
                (solveValue (ListV IntT [BoolV True, BoolV False]))
                initialState,

        testCase "Matchable with wrong type arguments" $
            expectedFailure
                (solveValue (ValueM [WordP "true", WordP "plus", WordP "false"]))
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
