module MatcherTest ( {-tests-} ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified ParserEnv as M
import qualified AST as T

--

{-
-- Assertions

-- Asserts that a matcher yields a specific result when matching a matchable with the given environment
expectedResult :: (HasCallStack, Eq b, Show b) => M.Env -> (a -> M.Matcher b) -> a -> b -> Assertion
expectedResult env m ms r =
    case M.match env m ms of
        Left e -> assertFailure $ "Matcher failed, the error was:\n" ++ e
        Right (r', _) ->
            if r == r'
            then return ()
            else assertFailure $ "The result was:\n" ++ show r' ++ "\nBut was expecting:\n" ++ show r

-- Asserts that a matcher fails to match a matchable with the given environment
expectedFailure :: (HasCallStack, Eq b, Show b) => M.Env -> (a -> M.Matcher b) -> a -> Assertion
expectedFailure env m ms =
    case M.match env m ms of
        Left _ -> return ()
        Right (r, _) -> assertFailure $ "Matcher didn't fail, the result was " ++ show r

emptyEnv :: M.Env
emptyEnv = ([], [])
--


-- Tests

asNameTests :: TestTree
asNameTests = testGroup "As name"
    [
        testCase "Many words" $
            expectedResult
                emptyEnv
                M.matchAsName
                [T.WordP "short", T.WordP "name"]
                (Just ["short", "name"]),

        testCase "Followed by number part" $
            expectedResult
                emptyEnv
                M.matchAsName
                [T.WordP "short", T.WordP "name", T.IntP 1]
                Nothing
    ]

--


-- Main

tests :: TestTree
tests = testGroup "Matcher"
    [
        asNameTests
    ]

--
-}
