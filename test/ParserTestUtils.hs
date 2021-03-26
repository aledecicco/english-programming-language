module ParserTestUtils where

import Test.Tasty.HUnit ( HasCallStack, assertFailure, Assertion, (@?=) )

import PreludeDefs ( operators, procedures )
import ParserEnv

--


-- Parser

emptyEnv :: ParserState
emptyEnv = ([], [], 0)

envWithFunctions :: ParserState
envWithFunctions = (operators ++ procedures, [], 0)

-- Asserts that a parser action yields a specific result with the given environment
expectedResult :: (HasCallStack, Eq a, Show a) => ParserEnv a -> ParserState -> a -> Assertion
expectedResult a s r =
    case runParserEnv a s of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ e
        Right (r', _) -> r' @?= r

-- Asserts that a parser action succeeds with the given environment
expectedSuccess :: (HasCallStack, Show a) => ParserEnv a -> ParserState -> Assertion
expectedSuccess a s =
    case runParserEnv a s of
        Left e -> assertFailure $ "Parser action failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that a parser action fails with the given environment
expectedFailure :: (HasCallStack, Show a) => ParserEnv a -> ParserState -> Assertion
expectedFailure a s =
    case runParserEnv a s of
        Left _ -> return ()
        Right (r, _) -> assertFailure $ "Parser action didn't fail, the result was " ++ show r

--
