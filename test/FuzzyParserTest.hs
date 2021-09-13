{-|
Module      : FuzzyParserTest
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The "FuzzyParser"'s test suite.
-}

module FuzzyParserTest (tests) where

import Control.Monad (void)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=), Assertion, HasCallStack)

import AST
import FuzzyParser


-- -----------------
-- * Assertions

-- | Asserts that a parser yields a specific result when parsing a given string.
expectedResult :: (HasCallStack, Eq a, Show a) => FuzzyParser a -> String -> a -> Assertion
expectedResult parser str expRes =
    case runFuzzyParser parser str of
        Left err -> assertFailure $ "Parser failed, the error was:\n" ++ show err
        Right res -> res @?= expRes

-- | Asserts that a parser yields a specific result ignoring annotations when parsing the given strings.
expectedBareResult :: (HasCallStack, Eq (a ()), Show (a ()), Functor a) => FuzzyParser (a b) -> String -> a () -> Assertion
expectedBareResult parser str expRes =
    case runFuzzyParser parser str of
        Left err -> assertFailure $ "Parser failed, the error was:\n" ++ show err
        Right res -> void res @?= expRes

-- | Asserts that a parser succeeds when parsing a given string.
expectedSuccess :: HasCallStack => FuzzyParser a -> String -> Assertion
expectedSuccess parser str =
    case runFuzzyParser parser str of
        Left err -> assertFailure $ "Parser failed, the error was:\n" ++ show err
        Right _ -> return ()

-- | Asserts that a parser fails to parse a given string.
expectedFailure :: (HasCallStack, Show a) => FuzzyParser a -> String -> Assertion
expectedFailure parser str =
    case runFuzzyParser parser str of
        Left _ -> return ()
        Right res -> assertFailure $ "Parser didn't fail, the result was:\n" ++ show res


-- -----------------
-- * Tests

anyWordTests :: TestTree
anyWordTests = testGroup "Any word"
    [
        testCase "Single word" $
            expectedResult
                anyWord
                "word"
                "word",

        testCase "Reserved word" $
            expectedResult
                anyWord
                "be"
                "be",

        testCase "Many words" $
            expectedResult
                anyWord
                "word and another word"
                "word",

        testCase "Followed by symbol" $
            expectedResult
                anyWord
                "word, another word"
                "word",

        testCase "Followed by number" $
            expectedFailure anyWord "word1"
    ]

nameTests :: TestTree
nameTests = testGroup "Name"
    [
        testCase "Single word" $
            expectedResult
                name
                "word"
                ["word"],

        testCase "Not reserved word" $
            expectedResult
                name
                "become"
                ["become"],

        testCase "Many words with reserved" $
            expectedResult
                name
                "word be another word"
                ["word"],

        testCase "Many words without reserved" $
            expectedResult
                name
                "word another word"
                ["word", "another", "word"],

        testCase "All caps reserved word" $
            expectedResult
                name
                "BE"
                ["BE"],

        testCase "Reserved word" $
            expectedFailure
                name
                "be"
    ]

typeNameTests :: TestTree
typeNameTests = testGroup "Type name"
    [
        testCase "Whole number" $
            expectedResult
                (typeName False)
                "whole number"
                IntT,

        testCase "Whole numbers" $
            expectedResult
                (typeName True)
                "whole numbers"
                IntT,

        testCase "List" $
            expectedResult
                (typeName False)
                "list of whole numbers"
                (ListT IntT),

        testCase "List of lists" $
            expectedResult
                (typeName False)
                "list of lists of whole numbers"
                (ListT (ListT IntT)),

        testCase "Reference" $
            expectedResult
                referenceType
                "reference to a list of numbers"
                (RefT (ListT FloatT)),

        testCase "List without element" $
            expectedFailure
                (typeName False)
                "list",

        testCase "List of lists without element" $
            expectedFailure
                (typeName False)
                "list of lists",

        testCase "Whole numbers without plural" $
            expectedFailure
                (typeName True)
                "whole number",

        testCase "Whole number with plural" $
            expectedFailure
                (typeName False)
                "whole numbers",

        testCase "List without plural element" $
            expectedFailure
                (typeName False)
                "list of whole number"
    ]

identifierTests :: TestTree
identifierTests = testGroup "Identifier"
    [
        testCase "Single word" $
            expectedResult
                identifier
                "word"
                "word",

        testCase "Not reserved word" $
            expectedResult
                identifier
                "become"
                "become",

        testCase "Many words with reserved" $
            expectedResult
                 identifier
                "word be another word"
                "word",

        testCase "Reserved word" $
            expectedFailure identifier "be"
    ]

reservedTests :: TestTree
reservedTests = testGroup "Reserved"
    [
        testCase "Single word" $
            expectedSuccess (word "word") "word",

        testCase "Reserved word" $
            expectedSuccess (word "be") "be",

        testCase "Many words" $
            expectedSuccess (word "word") "word be another word",

        testCase "Mismatching word" $
            expectedFailure (word "another") "word",

        testCase "Longer word" $
            expectedFailure (word "become") "be",

        testCase "Not reserved word" $
            expectedFailure (word "be") "become"
    ]

integerTests :: TestTree
integerTests = testGroup "Integer"
    [
        testCase "Positive" $
            expectedResult
                integer
                "11"
                11,

        testCase "Negative" $
            expectedResult
                integer
                "-11"
                (-11),

        testCase "Followed by symbol" $
            expectedResult
                integer
                "11: 12"
                11,

        testCase "Many numbers" $
            expectedResult
                integer
                "11 12"
                11,

        testCase "Not a float" $
            expectedResult
                integer
                "11.a"
                11,

        testCase "A float" $
            expectedResult
                integer
                "11.12"
                11,

        testCase "Word" $
            expectedFailure integer "word",

        testCase "Negative with space" $
            expectedFailure integer "- 11"
    ]

floatTests :: TestTree
floatTests = testGroup "Float"
    [
        testCase "Positive" $
            expectedResult
                float
                "11.12"
                11.12,

        testCase "Negative" $
            expectedResult
                float
                "-11.12"
                (-11.12),

        testCase "Many floats" $
            expectedResult
                float
                "11.12 12.13"
                11.12,

        testCase "Not a float" $
            expectedFailure float "11.a",

        testCase "A number" $
            expectedFailure float "11",

        testCase "Word" $
            expectedFailure float "word",

        testCase "With space after dot" $
            expectedFailure float "11. 12",

        testCase "With space before dot" $
            expectedFailure float "11 .12",

        testCase "Negative with space" $
            expectedFailure float "- 11.12"
    ]

parensTests :: TestTree
parensTests = testGroup "Parens"
    [
        testCase "Number" $
            expectedResult
                (parens integer)
                "(11)"
                11,

        testCase "Without parenthesis" $
            expectedFailure (parens integer) "11",

        testCase "Followed by symbol" $
            expectedFailure (parens integer) "(11,)",

        testCase "Not closed" $
            expectedFailure (parens integer) "(11"
    ]

seriesTests :: TestTree
seriesTests = testGroup "Series"
    [
        testCase "One number" $
            expectedResult
                (series integer)
                "11"
                [11],

        testCase "Two numbers" $
            expectedResult
                (series integer)
                "11, and 12"
                [11, 12],

        testCase "Three numbers" $
            expectedResult
                (series integer)
                "11, 12, and 13"
                [11, 12, 13],

        testCase "Without commas" $
            expectedResult
                (series integer)
                "11 12 and 13"
                [11],

        testCase "Double and" $
        expectedResult
            (series valueMatchable)
            "11, 12 and 13, and 14"
            [
                ValueM (1,1) [IntP (1,1) 11],
                ValueM (1,5) [IntP (1,5) 12, WordP (1,8) "and", IntP (1,12) 13],
                ValueM (1,20) [IntP (1,20) 14]
            ],

        testCase "Without last comma" $
            expectedFailure (series integer) "11, 12 13",

        testCase "Without and" $
            expectedFailure (series integer) "11, 12, 13",

        testCase "Not and" $
            expectedFailure (series integer) "11, 12, android 13"
    ]

intercalatedTests :: TestTree
intercalatedTests = testGroup "Intercalated"
    [
        testCase "Single correct element" $
            expectedResult
                (intercalated identifier anyWord)
                "word"
                ["word"],

        testCase "Even elements" $
            expectedResult
                (intercalated identifier anyWord)
                "word be another be"
                ["word", "be", "another", "be"],

        testCase "Odd elements" $
            expectedResult
                (intercalated identifier anyWord)
                "word be another be word"
                ["word", "be", "another", "be", "word"],

        testCase "Repeated even element" $
            expectedResult
                (intercalated identifier anyWord)
                "word be be another be"
                ["word", "be"],

        testCase "Repeated odd element" $
            expectedResult
                (intercalated identifier anyWord)
                "word be another another be"
                ["word", "be", "another", "another"],

        testCase "Single incorrect element" $
            expectedFailure (intercalated identifier anyWord) "be"
    ]

functionDefinitionTests :: TestTree
functionDefinitionTests = testGroup "Function definition"
    [
        testCase "Operator" $
            expectedResult
                functionDefinition
                "A number equal to the double of a number (m):\n  Let r be m times 2.\n  Return r."
                (FunDef (1,1)
                    (Title (1,19) [TitleWords (1,19) ["the", "double", "of"], TitleParam (1,33) [["m"]] FloatT])
                    (Just FloatT)
                    [
                        VarDef (2,3) [["r"]] Nothing (ValueM (2,12) [WordP (2,12) "m", WordP (2,14) "times", IntP (2,20) 2]),
                        Return (3,3) (ValueM (3,10) [WordP (3,10) "r"])
                    ]
                ),

        testCase "Procedure" $
            expectedResult
                functionDefinition
                "To double a number (m):\n  Let r be a number equal to m times 2."
                (FunDef (1,1)
                    (Title (1,4) [TitleWords (1,4) ["double"], TitleParam (1,11) [["m"]] FloatT])
                    Nothing
                    [
                        VarDef (2,3) [["r"]] (Just FloatT) (ValueM (2,30) [WordP (2,30) "m", WordP (2,32) "times", IntP (2,38) 2])
                    ]
                ),

        testCase "Predicate" $
            expectedResult
                functionDefinition
                "Whether a number (m) is whole:\n  Return false."
                (FunDef (1,1)
                    (Title (1,9) [TitleParam (1,9) [["m"]] FloatT, TitleWords (1,22) ["is", "whole"]])
                    (Just BoolT)
                    [
                        Return (2,3) (ValueM (2,10) [WordP (2,10) "false"])
                    ]
                ),

        testCase "Consecutive arguments in title" $
            expectedFailure
                functionDefinition
                "A number equal to the double of a number (m) a number (n):\n  Return m times n.",

        testCase "Operator without return type" $
            expectedFailure
                functionDefinition
                "The double of a number (m):\n  Let r be m times 2.\n  Return r."
    ]

titleTests :: TestTree
titleTests = testGroup "Title"
    [
        testCase "Words" $
            expectedResult
                title
                "Function definition"
                (Title (1,1) [TitleWords (1,1) ["Function", "definition"]]),

        testCase "Many parts" $
            expectedResult
                title
                "Definition with a number (m) and a number (n)"
                (Title (1,1) [
                    TitleWords (1,1) ["Definition", "with"],
                    TitleParam (1,17) [["m"]] FloatT,
                    TitleWords (1,30) ["and"],
                    TitleParam (1,34) [["n"]] FloatT
                ]),

        testCase "Consecutive arguments" $
            expectedResult
                title
                "Definition with a number (m) a number (n)"
                (Title (1,1) [
                    TitleWords (1,1) ["Definition", "with"],
                    TitleParam (1,17) [["m"]] FloatT
                ]),

        testCase "Missing name" $
            expectedResult
                title
                "Definition with a number"
                (Title (1,1) [
                    TitleWords (1,1) ["Definition", "with"],
                    TitleParam (1,17) [] FloatT
                ])
    ]

titleWordsTests :: TestTree
titleWordsTests = testGroup "Title words"
    [
        testCase "Words" $
            expectedBareResult
                titleWords
                "Function definition"
                (TitleWords () ["Function", "definition"]),

        testCase "Words followed by reserved word" $
            expectedBareResult
                titleWords
                "Function definition be"
                (TitleWords () ["Function", "definition", "be"]),

        testCase "Words followed by parameter" $
            expectedBareResult
                titleWords
                "Function definition a number"
                (TitleWords () ["Function", "definition"]),

        testCase "Reserved word first" $
            expectedBareResult
                titleWords
                "Be function definition"
                (TitleWords () ["Be", "function", "definition"])
    ]

titleParamTests :: TestTree
titleParamTests = testGroup "Title parameter"
    [
        testCase "Named" $
            expectedBareResult
                titleParam
                "a number (m)"
                (TitleParam () [["m"]] FloatT),

        testCase "Followed by words" $
            expectedBareResult
                titleParam
                "a number (m) function definition"
                (TitleParam () [["m"]] FloatT),

        testCase "Two parameters" $
            expectedBareResult
                titleParam
                "a number (m) a number (n)"
                (TitleParam () [["m"]] FloatT),

        testCase "Missing name" $
            expectedBareResult
                titleParam
                "a number"
                (TitleParam () [] FloatT),

        testCase "Missing article uppercase" $
            expectedFailure titleParam "number (m)",

        testCase "Missing article lowercase" $
            expectedFailure titleParam "number (m)"
    ]

listWithHeaderTests :: TestTree
listWithHeaderTests = testGroup "ListWithHeader"
    [
         testCase "One element" $
            expectedResult
                integerItems
                "Integers:\n  11."
                ("Integers", [11]),

        testCase "Many elements" $
            expectedResult
                integerItems
                "Integers:\n  11.\n  12."
                ("Integers", [11, 12]),

        testCase "Second element not indented" $
            expectedResult
                integerItems
                "Integers:\n  11.\n12."
                ("Integers", [11]),

        testCase "Nested lists" $
            expectedResult
                (listWithHeader anyWord integerItems)
                "Lists:\n  First:\n    11.\n    12.\n  Second:\n    13.\n    14."
                ("Lists", [("First", [11, 12]), ("Second", [13, 14])]),

        testCase "No elements" $
            expectedFailure integerItems "Integers:\n",

        testCase "Incorrect header" $
            expectedFailure integerItems "Integers11:\n  11.",

        testCase "First element not indented" $
            expectedFailure integerItems "Integers:\n11.",

        testCase "First element without dot" $
            expectedFailure integerItems "Integers:\n  11",

        testCase "Second element without dot" $
            expectedFailure integerItems "Integers:\n  11.\n  12"
    ]
    where
        integerItems :: FuzzyParser (String, [Int])
        integerItems = listWithHeader anyWord (integer <* dot)

valueTests :: TestTree
valueTests = testGroup "Value"
    [
        testCase "List" $
            expectedBareResult
                value
                "a list of numbers containing a, b, and c"
                (ListV () FloatT [ValueM () [WordP () "a"], ValueM () [WordP () "b"], ValueM () [WordP () "c"]]),

        testCase "Empty list" $
            expectedBareResult
                value
                "a list of numbers"
                (ListV () FloatT [])
    ]

tests :: TestTree
tests = testGroup "Parser"
    [
        anyWordTests,
        nameTests,
        typeNameTests,
        identifierTests,
        reservedTests,
        integerTests,
        floatTests,
        parensTests,
        seriesTests,
        intercalatedTests,
        functionDefinitionTests,
        titleTests,
        titleWordsTests,
        titleParamTests,
        listWithHeaderTests,
        valueTests
    ]
