module FuzzyParserTest ( tests ) where

import Test.Tasty ( testGroup, TestTree )
import Test.Tasty.HUnit ( HasCallStack, testCase, assertFailure, Assertion, (@?=) )

import FuzzyParser
import AST

--


-- Assertions

-- Asserts that a parser yields a specific result when parsing a given string
expectedResult :: (HasCallStack, Eq a, Show a) => FuzzyParser a -> String -> a -> Assertion
expectedResult p s r =
    case runFuzzyParser p s of
        Left e -> assertFailure $ "Parser failed, the error was:\n" ++ e
        Right r' -> r' @?= r

-- Asserts that a parser succeeds when parsing a given string
expectedSuccess :: HasCallStack => FuzzyParser a -> String -> Assertion
expectedSuccess p s =
    case runFuzzyParser p s of
        Left e -> assertFailure $ "Parser failed, the error was:\n" ++ e
        Right _ -> return ()

-- Asserts that a parser fails to parse a given string
expectedFailure :: (HasCallStack, Show a) => FuzzyParser a -> String -> Assertion
expectedFailure p s =
    case runFuzzyParser p s of
        Left _ -> return ()
        Right r -> assertFailure $ "Parser didn't fail, the result was " ++ show r

--


-- Tests

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
        testCase "Number" $
            expectedResult
                (typeName False)
                "number"
                IntT,

        testCase "Numbers" $
            expectedResult
                (typeName True)
                "numbers"
                IntT,

        testCase "List" $
            expectedResult
                (typeName False)
                "list of numbers"
                (ListT IntT),

        testCase "List of lists" $
            expectedResult
                (typeName False)
                "list of lists of numbers"
                (ListT (ListT IntT)),

        testCase "List without element" $
            expectedFailure
                (typeName False)
                "list",

        testCase "List of lists without element" $
            expectedFailure
                (typeName False)
                "list of lists",

        testCase "Numbers without plural" $
            expectedFailure
                (typeName True)
                "number",

        testCase "Number with plural" $
            expectedFailure
                (typeName False)
                "numbers",

        testCase "List without plural element" $
            expectedFailure
                (typeName False)
                "list of number"
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
                ValueM [IntP 11],
                ValueM [IntP 12, WordP "and", IntP 13],
                ValueM [IntP 14]
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
                "A number equal to the double of a number (m):\n  Let r be m times 2.\n  The result is r."
                (FunDef
                    (Line 1 [TitleWords ["the", "double", "of"], TitleParam ["m"] IntT])
                    (Just IntT)
                    [
                        Line 2 (VarDef [["r"]] (ValueM [WordP "m", WordP "times", IntP 2])),
                        Line 3 (Result (ValueM [WordP "r"]))
                    ]
                ),

        testCase "Procedure" $
            expectedResult
                functionDefinition
                "To double a number (m):\n  Let r be m times 2."
                (FunDef
                    (Line 1 [TitleWords ["double"], TitleParam ["m"] IntT])
                    Nothing
                    [
                        Line 2 (VarDef [["r"]] (ValueM [WordP "m", WordP "times", IntP 2]))
                    ]
                ),


        testCase "Predicate" $
            expectedResult
                functionDefinition
                "Whether a number (m) is whole:\n  The result is true."
                (FunDef
                    (Line 1 [TitleParam ["m"] IntT, TitleWords ["is", "whole"]])
                    (Just BoolT)
                    [
                        Line 2 (Result (ValueM [WordP "true"]))
                    ]
                ),

        testCase "Consecutive arguments in title" $
            expectedFailure
                functionDefinition
                "A number equal to the double of a number (m) a number (n):\n  The result is m times n.",

        testCase "Operator without return type" $
            expectedFailure
                functionDefinition
                "The double of a number (m):\n  Let r be m times 2.\n  The result is r."
    ]

titleTests :: TestTree
titleTests = testGroup "Title"
    [
        testCase "Words" $
            expectedResult
                title
                "Function definition"
                (Line 1 [TitleWords ["Function", "definition"]]),

        testCase "Many parts" $
            expectedResult
                title
                "Definition with a number (m) and a number (n)"
                (Line 1 [
                    TitleWords ["Definition", "with"],
                    TitleParam ["m"] IntT,
                    TitleWords ["and"],
                    TitleParam ["n"] IntT
                ]),

        testCase "Consecutive arguments" $
            expectedResult
                title
                "Definition with a number (m) a number (n)"
                (Line 1 [
                    TitleWords ["Definition", "with"],
                    TitleParam ["m"] IntT
                ]),

        testCase "Missing name" $
            expectedFailure title "Definition with a number"
    ]

titleWordsTests :: TestTree
titleWordsTests = testGroup "Title words"
    [
        testCase "Words" $
            expectedResult
                titleWords
                "Function definition"
                (TitleWords ["Function", "definition"]),

        testCase "Words followed by reserved word" $
            expectedResult
                titleWords
                "Function definition be"
                (TitleWords ["Function", "definition", "be"]),

        testCase "Words followed by parameter" $
            expectedResult
                titleWords
                "Function definition a number"
                (TitleWords ["Function", "definition"]),

        testCase "Reserved word first" $
            expectedResult
                titleWords
                "Be function definition"
                (TitleWords ["Be", "function", "definition"])
    ]

titleParamTests :: TestTree
titleParamTests = testGroup "Title parameter"
    [
        testCase "Named" $
            expectedResult
                (titleParam True)
                "A number (m)"
                (TitleParam ["m"] IntT),

        testCase "Followed by words" $
            expectedResult
                (titleParam True)
                "A number (m) function definition"
                (TitleParam ["m"] IntT),

        testCase "Two parameters" $
            expectedResult
                (titleParam True)
                "A number (m) a number (n)"
                (TitleParam ["m"] IntT),

        testCase "Missing name" $
            expectedFailure (titleParam True) "A number",

        testCase "Missing article uppercase" $
            expectedFailure (titleParam True) "number (m)",

        testCase "Missing article lowercase" $
            expectedFailure (titleParam False) "number (m)"
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
            expectedResult
                value
                "a list of numbers containing a, b, and c"
                (ListV IntT [ValueM [WordP "a"], ValueM [WordP "b"], ValueM [WordP "c"]]),


        testCase "Empty ist" $
            expectedResult
                value
                "a list of numbers"
                (ListV IntT [])
    ]

--


-- Main

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

--
