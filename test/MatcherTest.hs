module MatcherTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )

import TestUtils
import qualified ParserEnv as E
import qualified Matcher as M
import qualified AST as T

--


-- Tests

splitsTests :: TestTree
splitsTests = testGroup "Splits"
    [
        testCase "Addition" $
            M.splits [T.IntP 2, T.WordP "plus", T.IntP 3]
            @?=
            [
                ([T.IntP 2], [T.WordP "plus", T.IntP 3]),
                ([T.IntP 2, T.WordP "plus"], [T.IntP 3]),
                ([T.IntP 2, T.WordP "plus", T.IntP 3], [])
            ],

        testCase "Addition with recursive arguments" $
            M.splits [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]
            @?=
            [
                ([T.IntP 2], [T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]),
                ([T.IntP 2, T.WordP "times"], [T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]),
                ([T.IntP 2, T.WordP "times", T.IntP 3], [T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]),
                ([T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus"], [T.IntP 4, T.WordP "times", T.IntP 5]),
                ([T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4], [T.WordP "times", T.IntP 5]),
                ([T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times"], [T.IntP 5]),
                ([T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5], [])
            ]
    ]

sepByTitleTests :: TestTree
sepByTitleTests = testGroup "Sep by title"
    [
        testCase "Addition" $
            M.sepByTitle
                [T.IntP 2, T.WordP "plus", T.IntP 3]
                [T.TitleParam ["m"] T.FloatT, T.TitleWords ["plus"], T.TitleParam ["n"] T.FloatT]
            @?=
            [[[T.IntP 2], [T.IntP 3]]],

        testCase "Addition with recursive arguments" $
            M.sepByTitle
                [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]
                [T.TitleParam ["m"] T.FloatT, T.TitleWords ["plus"], T.TitleParam ["n"] T.FloatT]
            @?=
            [[[T.IntP 2, T.WordP "times", T.IntP 3],[T.IntP 4, T.WordP "times", T.IntP 5]]],

        testCase "Multiplication with recursive arguments" $
            M.sepByTitle
                [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]
                [T.TitleParam ["m"] T.FloatT, T.TitleWords ["times"], T.TitleParam ["n"] T.FloatT]
            @?=
            [
                [[T.IntP 2], [T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5]],
                [[T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4], [T.IntP 5]]
            ]
    ]

asNameTests :: TestTree
asNameTests = testGroup "As name"
    [
        testCase "Many words" $
            expectedResult
                (M.matchAsName [T.WordP "short", T.WordP "name"])
                emptyEnv
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedResult
                (M.matchAsName [T.WordP "short", T.WordP "name", T.IntP 1])
                emptyEnv
                Nothing
    ]

asFunctionCallTests :: TestTree
asFunctionCallTests = testGroup "As function call"
    [
        testCase "Arguments at beggining and end" $
            expectedResult
                (M.matchAsFunctionCall [T.IntP 2, T.WordP "plus", T.IntP 3])
                envWithFunctions
                (Just ("%_plus_%", [T.IntV 2, T.IntV 3])),

        testCase "Addition and multiplication associativity" $
            expectedResult
                (M.matchAsFunctionCall [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4, T.WordP "times", T.IntP 5])
                envWithFunctions
                (Just ("%_plus_%", [T.OperatorCall "%_times_%" [T.IntV 2, T.IntV 3], T.OperatorCall "%_times_%" [T.IntV 4, T.IntV 5]])),

        testCase "Forced associativity with parenthesis" $
            expectedResult
                (M.matchAsFunctionCall [T.ParensP [T.IntP 2, T.WordP "times", T.IntP 3, T.WordP "plus", T.IntP 4], T.WordP "times", T.IntP 5])
                envWithFunctions
                (Just ("%_times_%", [T.OperatorCall "%_plus_%" [T.OperatorCall "%_times_%" [T.IntV 2, T.IntV 3], T.IntV 4], T.IntV 5])),

        testCase "Wrong type arguments" $
            expectedResult
                (M.matchAsFunctionCall [T.WordP "true", T.WordP "plus", T.WordP "false"])
                envWithFunctions
                (Just ("%_plus_%", [T.BoolV True, T.BoolV False]))
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
