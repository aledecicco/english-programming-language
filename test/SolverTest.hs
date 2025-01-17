{-|
Module      : SolverTest
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The "Solver"'s test suite.
-}

module SolverTest (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), Assertion, HasCallStack)

import AST
import BuiltInDefs (builtInOperators, builtInProcedures)
import Errors
import Matchers
import Solver
import SolverEnv


-- -----------------
-- * Assertions

-- | Asserts that an action yields a specific result.
expectedResult :: (HasCallStack, Eq a, Show a) => SolverEnv a -> a -> Assertion
expectedResult a r =
    case runSolverEnv a [] initialLocation initialState of
        (Left e, _) -> assertFailure $ "Parser action failed, the error was:\n" ++ show e
        (Right ((r', _), _), _) -> r' @?= r

-- | Asserts that an action succeeds.
expectedSuccess :: (HasCallStack, Show a) => SolverEnv a -> Assertion
expectedSuccess a =
    case runSolverEnv a [] initialLocation initialState of
        (Left e, _) -> assertFailure $ "Parser action failed, the error was:\n" ++ show e
        (Right _, _) -> return ()

-- | Asserts that an action yields a specific error.
expectedError :: (HasCallStack, Show a) => SolverEnv a -> Error -> Assertion
expectedError a e =
    case runSolverEnv a [] initialLocation initialState of
        (Left e', _) -> e' @?= e
        (Right ((res, _), _), _) -> assertFailure $ "Parser action didn't fail, the result was " ++ show res


-- -----------------
-- * Tests

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
                [TitleParam () [] FloatT, TitleWords () ["plus"], TitleParam () [] FloatT]
            @?=
            [[[IntP () 2], [IntP () 3]]],

        testCase "Addition with recursive arguments" $
            sepByTitle
                [IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]
                [TitleParam () [] FloatT, TitleWords () ["plus"], TitleParam () [] FloatT]
            @?=
            [[[IntP () 2, WordP () "times", IntP () 3],[IntP () 4, WordP () "times", IntP () 5]]],

        testCase "Multiplication with recursive arguments" $
            sepByTitle
                [IntP () 2, WordP () "times", IntP () 3, WordP () "plus", IntP () 4, WordP () "times", IntP () 5]
                [TitleParam () [] FloatT, TitleWords () ["times"], TitleParam () [] FloatT]
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
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedResult
                (matchAsName [WordP () "short", WordP () "name", IntP () 1])
                Nothing
    ]

asFunctionCallTests :: TestTree
asFunctionCallTests = testGroup "As function call"
    [
        testCase "Arguments at beggining and end" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3] $ map snd builtInOperators)
                [("%_plus_%", [IntV (0,0) 2, IntV (0,7) 3])],

        testCase "Addition and multiplication associativity" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5] $ map snd builtInOperators)
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
                [
                    ("%_times_%", [OperatorCall (0,1) "%_plus_%" [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], IntV (0,16) 4], IntV (0,25) 5]),
                    ("%_times_%", [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, OperatorCall (0,9) "%_plus_%" [IntV (0,9) 3, IntV (0,16) 4]], IntV (0,25) 5])
                ],

        testCase "Wrong type arguments" $
            expectedResult
                (matchAsFunctionCall [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"] $ map snd builtInOperators)
                [("%_plus_%", [BoolV (0,0) True, BoolV (0,10) False])],

        testCase "Wrong functions category" $
            expectedResult
                (matchAsFunctionCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3] $ map snd builtInProcedures)
                []
    ]

asOperatorCallTests :: TestTree
asOperatorCallTests = testGroup "As operator call"
    [
        testCase "Procedure matchable" $
            expectedResult
                (matchAsOperatorCall [WordP (0,0) "print", IntP (0,6) 3])
                []
    ]

asProcedureCallTests :: TestTree
asProcedureCallTests = testGroup "As procedure call"
    [
        testCase "Operator matchable" $
            expectedResult
                (matchAsProcedureCall [IntP (0,0) 2, WordP (0,2) "plus", IntP (0,7) 3])
                []
    ]

getValueTypeTests :: TestTree
getValueTypeTests = testGroup "Get value type"
    [
        testCase "Adding ints" $
            expectedResult
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, IntV (0,0) 3])
                IntT,

        testCase "Adding mixed" $
            expectedResult
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, FloatV (0,0) 3.0])
                FloatT,

        testCase "Correct type arguments" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 2, FloatV (0,6) 3.0]),

        testCase "Correct bound types" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) IntT [IntV (0,1) 1, IntV (0,3) 2], ListV (0,17) IntT [IntV (0,18) 3, IntV (0,20) 4]]),

        testCase "Satisfiable bound types" $
            expectedSuccess
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) FloatT [FloatV (0,1) 1, IntV (0,3) 2], ListV (0,17) IntT [IntV (0,18) 3, IntV (0,20) 4]]),

        testCase "Wrong bound types" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) BoolT [BoolV (0,1) True, BoolV (0,7) False], ListV (0,21) IntT [IntV (0,22) 3, IntV (0,24) 4]])
                (Error (Just (0,21)) $ WrongTypeArgument (ListT BoolT) (ListT IntT) 1 "%_appended_to_%"),

        testCase "Wrong type arguments" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_plus_%" [IntV (0,0) 1, BoolV (0,3) True])
                (Error (Just (0,3)) $ WrongTypeArgument FloatT BoolT 1 "%_plus_%"),

        testCase "Wrong type lists" $
            expectedError
                (getValueType $ OperatorCall (0,0) "%_appended_to_%" [ListV (0,0) BoolT [BoolV (0,1) True, BoolV (0,6) False], ListV (0,20) IntT [IntV (0,21) 3, IntV (0,23) 4]])
                (Error (Just (0,20)) $ WrongTypeArgument (ListT BoolT) (ListT IntT) 1 "%_appended_to_%")
    ]

setVariableTypeTests :: TestTree
setVariableTypeTests = testGroup "Set variable type"
    [
        testCase "Repeated new variable" $
            expectedError
                (setVariableType ["a"] IntT >> setNewVariableType ["a"] IntT)
                (Error (Just (0,0)) $ VariableAlreadyDefined ["a"])
    ]

solveValueTests :: TestTree
solveValueTests = testGroup "Solve value"
    [
        testCase "Matchable" $
            expectedResult
                (solveValueWithType IntT False $ ValueM (0,0) [IntP (0,0) 2, WordP (0,2) "times", IntP (0,8) 3, WordP (0,10) "plus", IntP (0,15) 4, WordP (0,17) "times", IntP (0,23) 5])
                (OperatorCall (0,0) "%_plus_%" [OperatorCall (0,0) "%_times_%" [IntV (0,0) 2, IntV (0,8) 3], OperatorCall (0,15) "%_times_%" [IntV (0,15) 4, IntV (0,23) 5]]),

        testCase "List" $
            expectedResult
                (solveValueWithType (ListT IntT) False $  ListV (0,0) IntT [ValueM (0,1) [IntP (0,1) 2, WordP (0,3) "times", IntP (0,9) 3], ValueM (0,11) [IntP (0,11) 4, WordP (0,13) "times", IntP (0,19) 5]])
                (ListV (0,0) IntT [OperatorCall (0,1) "%_times_%" [IntV (0,1) 2, IntV (0,9) 3], OperatorCall (0,11) "%_times_%" [IntV (0,11) 4, IntV (0,19) 5]]),

        testCase "Ambiguous value" $
            expectedResult
                (do
                    setVariableType ["L"] (ListT IntT)
                    solveValueWithType
                        IntT
                        False
                        (ValueM (0,0) [WordP (0,0) "the", WordP (0,4) "element", WordP (0,12) "of", WordP (0,15) "L", WordP (0,17) "at", IntP (0,20) 2, WordP (0,22) "plus", IntP (0,27) 2])
                )
                (OperatorCall (0,0) "the_element_of_%_at_%" [VarV (0,15) ["L"],OperatorCall (0,20) "%_plus_%" [IntV (0,20) 2,IntV (0,27) 2]]),

        testCase "Disambiguated value" $
            expectedResult
                (do
                    setVariableType ["L"] (ListT CharT)
                    solveValueWithType
                        CharT
                        False
                        (ValueM (0,0) [WordP (0,0) "the", WordP (0,4) "element", WordP (0,12) "of", WordP (0,15) "L", WordP (0,17) "at", IntP (0,20) 2, WordP (0,22) "plus", IntP (0,27) 2])
                )
                (OperatorCall (0,0) "the_element_of_%_at_%" [VarV (0,15) ["L"], OperatorCall (0,20) "%_plus_%" [IntV (0,20) 2, IntV (0,27) 2]]),

        testCase "Matchable with wrong type arguments" $
            expectedError
                (solveValueWithType IntT False $ ValueM (0,0) [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false"])
                (Error (Just (0,0)) (WrongTypeArgument FloatT BoolT 0 "%_plus_%")),

        testCase "Ambiguous matchable with wrong type arguments" $
            expectedError
                (solveValueWithType IntT False $ ValueM (0,0) [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false", WordP (0,16) "plus", WordP (0,21) "true"])
                (Error (Just (0,0)) (UnmatchableValueTypes [WordP (0,0) "true", WordP (0,5) "plus", WordP (0,10) "false", WordP (0,16) "plus", WordP (0,21) "true"])),

        testCase "List with wrong items" $
            expectedError
                (solveValueWithType (ListT IntT) False $ ListV (0,0) IntT [BoolV (0,1) True, BoolV (0,7) False])
                (Error (Just (0,1)) $ WrongTypeValue IntT BoolT)
    ]

solveSentenceTests :: TestTree
solveSentenceTests = testGroup "Solve sentence"
    [
        testCase "Read into list position" $
            expectedSuccess
                (do
                    setVariableType ["L"] (ListT IntT)
                    solveSentence Nothing $ Read (0,0) IntT (OperatorCall (0,25) "the_element_of_%_at_%" [VarV (0,40) ["L"], IntV (0,45) 1])
                ),

        testCase "Read wrong type" $
            expectedError
                (do
                    setVariableType ["L"] (ListT IntT)
                    solveSentence Nothing $ Read (0,0) CharT (OperatorCall (0,37) "the_element_of_%_at_%" [VarV (0,52) ["L"], IntV (0,57) 1])
                )
                (Error (Just (0,37)) $ WrongTypeValue (RefT CharT) (RefT IntT)),

        testCase "Read complex type" $
            expectedError
                (do
                    setVariableType ["L"] (ListT IntT)
                    solveSentence Nothing $ Read (0,0) (ListT $ ListT CharT) (OperatorCall (0,40) "the_element_of_%_at_%" [VarV (0,55) ["L"], IntV (0,60) 1])
                )
                (Error (Just (0,0)) $ UnreadableType (ListT $ ListT CharT))
    ]

addAliasesTests :: TestTree
addAliasesTests = testGroup "Add aliases"
    [
        testCase "List with lists of strings" $
            addAliases [TitleParam () [] (ListT IntT), TitleParam () [] (ListT $ ListT CharT), TitleParam () [] (ListT $ ListT CharT)]
            @?=
            [
                TitleParam () [["the", "1st", "list"], ["the", "list", "of", "whole", "numbers"]] (ListT IntT),
                TitleParam () [["the", "2nd", "list"], ["the", "1st", "list", "of", "strings"], ["the", "1st", "list", "of", "lists"], ["the", "1st", "list", "of", "lists", "of", "characters"]] (ListT $ ListT CharT),
                TitleParam () [["the", "3rd", "list"], ["the", "2nd", "list", "of", "strings"], ["the", "2nd", "list", "of", "lists"], ["the", "2nd", "list", "of", "lists", "of", "characters"]] (ListT $ ListT CharT)
            ],

        testCase "List with lists of strings without collision" $
            addAliases [TitleParam () [] (ListT IntT), TitleParam () [] (ListT $ ListT CharT), TitleParam () [["L"]] (ListT $ ListT CharT)]
            @?=
            [
                TitleParam () [["the", "1st", "list"], ["the", "list", "of", "whole", "numbers"]] (ListT IntT),
                TitleParam () [["the", "2nd", "list"], ["the", "1st", "list", "of", "strings"], ["the", "1st", "list", "of", "lists"], ["the", "1st", "list", "of", "lists", "of", "characters"]] (ListT $ ListT CharT),
                TitleParam () [["L"]] (ListT $ ListT CharT)
            ],

        testCase "List with lists of strings with collision with \"the\"" $
            addAliases [TitleParam () [] (ListT IntT), TitleParam () [] (ListT $ ListT CharT), TitleParam () [["the", "1st", "list", "of", "strings"]] (ListT $ ListT CharT)]
            @?=
            [
                TitleParam () [["the", "1st", "list"], ["the", "list", "of", "whole", "numbers"]] (ListT IntT),
                TitleParam () [["the", "2nd", "list"], ["the", "1st", "list", "of", "lists"], ["the", "1st", "list", "of", "lists", "of", "characters"]] (ListT $ ListT CharT),
                TitleParam () [["the", "1st", "list", "of", "strings"]] (ListT $ ListT CharT)
            ],

        testCase "List with lists of strings with collision without \"the\"" $
            addAliases [TitleParam () [] (ListT IntT), TitleParam () [] (ListT $ ListT CharT), TitleParam () [["1st", "list", "of", "strings"]] (ListT $ ListT CharT)]
            @?=
            [
                TitleParam () [["the", "1st", "list"], ["the", "list", "of", "whole", "numbers"]] (ListT IntT),
                TitleParam () [["the", "2nd", "list"], ["the", "1st", "list", "of", "lists"], ["the", "1st", "list", "of", "lists", "of", "characters"]] (ListT $ ListT CharT),
                TitleParam () [["1st", "list", "of", "strings"]] (ListT $ ListT CharT)
            ]
    ]

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
        solveValueTests,
        solveSentenceTests,
        addAliasesTests
    ]
