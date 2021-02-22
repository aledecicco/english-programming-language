module MatcherTest ( tests ) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Matcher as M
import qualified Types as T

--


-- Assertions

match :: M.Matcher a -> [T.MatchablePart] -> Either M.Error a
match m ms = undefined

-- Asserts that a matcher yields a specific result when matching a matchable with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => M.Matcher a -> M.Env -> [T.MatchablePart] -> Assertion
expectedResult m e ms = undefined

-- Asserts that a matcher fails to match a matchable with the given environment
expectedFailure :: (HasCallStack, Eq a, Show a) =>  M.Matcher a -> M.Env -> [T.MatchablePart] -> Assertion
expectedFailure p s = undefined

--


-- Tests

--


-- Main

tests :: TestTree
tests = testGroup "Matcher" []

--
