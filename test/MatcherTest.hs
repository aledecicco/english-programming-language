module MatcherTest ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified PreludeDefs as D
import qualified ParserEnv as E
import qualified Matcher as M
import qualified AST as T

--

-- Assertions

-- Asserts that a matcher yields a specific result when matching a matchable with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => E.ParserEnv a -> E.ParserState -> a -> Assertion
expectedResult m s r =
    case E.runParserEnv m s of
        Left e -> assertFailure $ "Matcher failed, the error was:\n" ++ e
        Right (r', _) ->
            if r == r'
            then return ()
            else assertFailure $ "The result was:\n" ++ show r' ++ "\nBut was expecting:\n" ++ show r

-- Asserts that a matcher fails to match a matchable with the given environment
expectedFailure :: (HasCallStack, Show a) => E.ParserEnv a -> E.ParserState -> Assertion
expectedFailure m s =
    case E.runParserEnv m s of
        Left _ -> return ()
        Right (r, _) -> assertFailure $ "Matcher didn't fail, the result was " ++ show r

--


-- Auxiliary

emptyEnv :: E.ParserState
emptyEnv = ([], [], 0)

envWithFunctions :: E.ParserState
envWithFunctions = (D.operators ++ D.procedures, [], 0)

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
            [[[T.IntP 2, T.WordP "times", T.IntP 3],[T.IntP 4, T.WordP "times", T.IntP 5]]]
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
                (Just ("%_plus_%", [T.OperatorCall "%_times_%" [T.IntV 2, T.IntV 3], T.OperatorCall "%_times_%" [T.IntV 4, T.IntV 5]]))
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
