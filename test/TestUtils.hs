module TestUtils where

import Test.Tasty.HUnit ( HasCallStack, assertFailure, Assertion, (@?=) )

import qualified PreludeDefs as D
import qualified ParserEnv as E

--


-- Parser

emptyEnv :: E.ParserState
emptyEnv = ([], [], 0)

envWithFunctions :: E.ParserState
envWithFunctions = (D.operators ++ D.procedures, [], 0)

-- Asserts that an action yields a specific result with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => E.ParserEnv a -> E.ParserState -> a -> Assertion
expectedResult a s r =
    case E.runParserEnv a s of
        Left e -> assertFailure $ "Action failed, the error was:\n" ++ e
        Right (r', _) -> r' @?= r

-- Asserts that an action succeeds with the given environment
expectedSuccess :: (HasCallStack, Show a) => E.ParserEnv a -> E.ParserState -> Assertion
expectedSuccess a s =
    case E.runParserEnv a s of
        Left e -> assertFailure $ "Action failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that an action fails with the given environment
expectedFailure :: (HasCallStack, Show a) => E.ParserEnv a -> E.ParserState -> Assertion
expectedFailure a s =
    case E.runParserEnv a s of
        Left _ -> return ()
        Right (r, _) -> assertFailure $ "Action didn't fail, the result was " ++ show r

--
