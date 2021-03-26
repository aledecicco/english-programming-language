module MatcherTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import ParserTestUtils
import Matcher
import AST

--


-- Tests

splitsTests :: TestTree
splitsTests = testGroup "Splits"
    [
        testCase "Addition" $
            splits [IntP 2, WordP "plus", IntP 3]
            @?=
            [
                ([IntP 2], [WordP "plus", IntP 3]),
                ([IntP 2, WordP "plus"], [IntP 3]),
                ([IntP 2, WordP "plus", IntP 3], [])
            ],

        testCase "Addition with recursive arguments" $
            splits [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]
            @?=
            [
                ([IntP 2], [WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]),
                ([IntP 2, WordP "times"], [IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]),
                ([IntP 2, WordP "times", IntP 3], [WordP "plus", IntP 4, WordP "times", IntP 5]),
                ([IntP 2, WordP "times", IntP 3, WordP "plus"], [IntP 4, WordP "times", IntP 5]),
                ([IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4], [WordP "times", IntP 5]),
                ([IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times"], [IntP 5]),
                ([IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5], [])
            ]
    ]

sepByTitleTests :: TestTree
sepByTitleTests = testGroup "Sep by title"
    [
        testCase "Addition" $
            sepByTitle
                [IntP 2, WordP "plus", IntP 3]
                [TitleParam ["m"] FloatT, TitleWords ["plus"], TitleParam ["n"] FloatT]
            @?=
            [[[IntP 2], [IntP 3]]],

        testCase "Addition with recursive arguments" $
            sepByTitle
                [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]
                [TitleParam ["m"] FloatT, TitleWords ["plus"], TitleParam ["n"] FloatT]
            @?=
            [[[IntP 2, WordP "times", IntP 3],[IntP 4, WordP "times", IntP 5]]],

        testCase "Multiplication with recursive arguments" $
            sepByTitle
                [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]
                [TitleParam ["m"] FloatT, TitleWords ["times"], TitleParam ["n"] FloatT]
            @?=
            [
                [[IntP 2], [IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5]],
                [[IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4], [IntP 5]]
            ]
    ]

asNameTests :: TestTree
asNameTests = testGroup "As name"
    [
        testCase "Many words" $
            expectedResult
                (matchAsName [WordP "short", WordP "name"])
                emptyEnv
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedResult
                (matchAsName [WordP "short", WordP "name", IntP 1])
                emptyEnv
                Nothing
    ]

asFunctionCallTests :: TestTree
asFunctionCallTests = testGroup "As function call"
    [
        testCase "Arguments at beggining and end" $
            expectedResult
                (matchAsFunctionCall [IntP 2, WordP "plus", IntP 3])
                envWithFunctions
                (Just ("%_plus_%", [IntV 2, IntV 3])),

        testCase "Addition and multiplication associativity" $
            expectedResult
                (matchAsFunctionCall [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4, WordP "times", IntP 5])
                envWithFunctions
                (Just ("%_plus_%", [OperatorCall "%_times_%" [IntV 2, IntV 3], OperatorCall "%_times_%" [IntV 4, IntV 5]])),

        testCase "Forced associativity with parenthesis" $
            expectedResult
                (matchAsFunctionCall [ParensP [IntP 2, WordP "times", IntP 3, WordP "plus", IntP 4], WordP "times", IntP 5])
                envWithFunctions
                (Just ("%_times_%", [OperatorCall "%_plus_%" [OperatorCall "%_times_%" [IntV 2, IntV 3], IntV 4], IntV 5])),

        testCase "Wrong type arguments" $
            expectedResult
                (matchAsFunctionCall [WordP "true", WordP "plus", WordP "false"])
                envWithFunctions
                (Just ("%_plus_%", [BoolV True, BoolV False]))
    ]

--


-- Main

tests :: TestTree
tests = testGroup "Matcher"
    [
        splitsTests,
        sepByTitleTests,
        asNameTests,
        asFunctionCallTests
    ]

--
