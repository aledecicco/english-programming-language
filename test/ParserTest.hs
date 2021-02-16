module ParserTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

import qualified Parser as P
import qualified Types as T

--


-- Assertions

-- Asserts that a parser yields a specific result when parsing a given string
expectedResult :: (HasCallStack, Eq a, Show a) => P.Parser a -> String -> a -> Assertion
expectedResult p s r =
    case parse p "" s of
        Left e -> assertFailure $ "Parser failed, the error was:\n" ++ errorBundlePretty e
        Right r' -> if r == r' then return () else assertFailure $ "The result was:\n" ++ show r' ++ "\nBut was expecting:\n" ++ show r

-- Asserts that a parser succeeds when parsing a given string
expectedSuccess :: (HasCallStack, Eq a, Show a) => P.Parser a -> String -> Assertion
expectedSuccess p s =
    case parse p "" s of
        Left e -> assertFailure $ "Parser failed, the error was:\n" ++ errorBundlePretty e
        Right _ -> return ()

-- Asserts that a parser fails to parse a given string
expectedFailure :: (HasCallStack, Eq a, Show a) => P.Parser a -> String -> Assertion
expectedFailure p s =
    case parse p "" s of
        Left _ -> return ()
        Right r -> assertFailure $ "Parser didn't fail, the result was " ++ show r

--


-- Tests

anyWordTests :: TestTree
anyWordTests = testGroup "Any word"
    [
        testCase "Single word" $
            expectedResult
                P.anyWord
                "word"
                "word",

        testCase "Reserved word" $
            expectedResult
                P.anyWord
                "and"
                "and",

        testCase "Many words" $
            expectedResult
                P.anyWord
                "word and another word"
                "word",

        testCase "Followed by symbol" $
            expectedResult
                P.anyWord
                "word, another word"
                "word",

        testCase "Followed by number" $
            expectedFailure P.anyWord "word1"
    ]

nameTests :: TestTree
nameTests = testGroup "Name"
    [
        testCase "Single word" $
            expectedResult
                P.name
                "word"
                ["word"],

        testCase "Not reserved word" $
            expectedResult
                P.name
                "android"
                ["android"],

        testCase "Many words with reserved" $
            expectedResult
                P.name
                "word and another word"
                ["word"],

        testCase "Many words without reserved" $
            expectedResult
                P.name
                "word another word"
                ["word", "another", "word"],

        testCase "Reserved word" $
            expectedFailure
                P.name
                "and"
    ]


typeNameTests :: TestTree
typeNameTests = testGroup "Type name"
    [
        testCase "Integer" $
            expectedResult
                (P.typeName False)
                "integer"
                T.IntT,

        testCase "Integers" $
            expectedResult
                (P.typeName True)
                "integers"
                T.IntT,

        testCase "List" $
            expectedResult
                (P.typeName False)
                "list of integers"
                (T.ListT T.IntT),

        testCase "List of lists" $
            expectedResult
                (P.typeName False)
                "list of lists of integers"
                (T.ListT (T.ListT T.IntT)),

        testCase "List without element" $
            expectedFailure
                (P.typeName False)
                "list",

        testCase "List of lists without element" $
            expectedFailure
                (P.typeName False)
                "list of lists",

        testCase "Integers without plural" $
            expectedFailure
                (P.typeName True)
                "integer",

        testCase "Integer with plural" $
            expectedFailure
                (P.typeName False)
                "integers",

        testCase "List without plural element" $
            expectedFailure
                (P.typeName False)
                "list of integer"
    ]

identifierTests :: TestTree
identifierTests = testGroup "Identifier"
    [
        testCase "Single word" $
            expectedResult
                P.identifier
                "word"
                "word",

        testCase "Not reserved word" $
            expectedResult
                P.identifier
                "android"
                "android",

        testCase "Many words" $
            expectedResult
                 P.identifier
                "word and another word"
                "word",

        testCase "Reserved word" $
            expectedFailure P.identifier "and"
    ]

reservedTests :: TestTree
reservedTests = testGroup "Reserved"
    [
        testCase "Single word" $
            expectedSuccess (P.reserved "word") "word",

        testCase "Reserved word" $
            expectedSuccess (P.reserved "and") "and",

        testCase "Many words" $
            expectedSuccess (P.reserved "word") "word and another word",

        testCase "Mismatching word" $
            expectedFailure (P.reserved "another") "word",

        testCase "Longer word" $
            expectedFailure (P.reserved "android") "and",

        testCase "Not reserved word" $
            expectedFailure (P.reserved "and") "android"
    ]

integerTests :: TestTree
integerTests = testGroup "Integer"
    [
        testCase "Number" $
            expectedResult
                P.integer
                "11"
                11,

        testCase "Negative" $
            expectedResult
                P.integer
                "-11"
                (-11),

        testCase "Followed by symbol" $
            expectedResult
                P.integer
                "11: 12"
                11,

        testCase "Many numbers" $
            expectedResult
                P.integer
                "11 12"
                11,

        testCase "Not a float" $
            expectedResult
                P.integer
                "11.a"
                11,

        testCase "A float" $
            expectedResult
                P.integer
                "11.12"
                11,

        testCase "Word" $
            expectedFailure P.integer "word",

        testCase "Negative with space" $
            expectedFailure P.integer "- 11"
    ]


floatTests :: TestTree
floatTests = testGroup "Integer"
    [
        testCase "Number" $
            expectedResult
                P.float
                "11.12"
                11.12,

        testCase "Negative" $
            expectedResult
                P.float
                "-11.12"
                (-11.12),

        testCase "Many numbers" $
            expectedResult
                P.float
                "11.12 12.13"
                11.12,

        testCase "Not a float" $
            expectedFailure P.float "11.a",

        testCase "An integer" $
            expectedFailure P.float "11",

        testCase "Word" $
            expectedFailure P.float "word",

        testCase "With space after dot" $
            expectedFailure P.float "11. 12",

        testCase "With space before dot" $
            expectedFailure P.float "11 .12",

        testCase "Negative with space" $
            expectedFailure P.float "- 11.12"
    ]

parensTests :: TestTree
parensTests = testGroup "Parens"
    [
        testCase "Number" $
            expectedResult
                (P.parens P.integer)
                "(11)"
                11,

        testCase "Without parenthesis" $
            expectedFailure (P.parens P.integer) "11",

        testCase "Followed by symbol" $
            expectedFailure (P.parens P.integer) "(11,)",

        testCase "Not closed" $
            expectedFailure (P.parens P.integer) "(11"
    ]

seriesTests :: TestTree
seriesTests = testGroup "Series"
    [
        testCase "One number" $
            expectedResult
                (P.series P.integer)
                "11"
                [11],

        testCase "Two numbers" $
            expectedResult
                (P.series P.integer)
                "11, and 12"
                [11, 12],

        testCase "Three numbers" $
            expectedResult
                (P.series P.integer)
                "11, 12, and 13"
                [11, 12, 13],


        testCase "Without commas" $
            expectedResult
                (P.series P.integer)
                "11 12 and 13"
                [11],

        testCase "Double and" $
        expectedResult
            (P.series P.valueMatchable)
            "11, 12 and 13, and 14"
            [
                T.ValueM [T.IntP 11],
                T.ValueM [T.IntP 12, T.WordP "and", T.IntP 13],
                T.ValueM [T.IntP 14]
            ],

        testCase "Without last comma" $
            expectedFailure (P.series P.integer) "11, 12 13",

        testCase "Without and" $
            expectedFailure (P.series P.integer) "11, 12, 13",

        testCase "Not and" $
            expectedFailure (P.series P.integer) "11, 12, android 13"
    ]

intercalatedTests :: TestTree
intercalatedTests = testGroup "Intercalated"
    [
        testCase "Single correct element" $
            expectedResult
                (P.intercalated P.identifier P.anyWord)
                "word"
                ["word"],

        testCase "Even elements" $
            expectedResult
                (P.intercalated P.identifier P.anyWord)
                "word and another and"
                ["word", "and", "another", "and"],

        testCase "Odd elements" $
            expectedResult
                (P.intercalated P.identifier P.anyWord)
                "word and another and word"
                ["word", "and", "another", "and", "word"],

        testCase "Repeated even element" $
            expectedResult
                (P.intercalated P.identifier P.anyWord)
                "word and and another and"
                ["word", "and"],

        testCase "Repeated odd element" $
            expectedResult
                (P.intercalated P.identifier P.anyWord)
                "word and another another and"
                ["word", "and", "another", "another"],

        testCase "Single incorrect element" $
            expectedFailure (P.intercalated P.identifier P.anyWord) "and"
    ]

functionDefinitionTests :: TestTree
functionDefinitionTests = testGroup "Function definition"
    [
        testCase "Many sentences" $
            expectedResult
                P.functionDefinition
                "The double of an integer (m):\n  Let r be m times 2.\n  The result is r."
                (T.FunDef
                    [T.TitleWords ["The", "double", "of"], T.TitleParam ["m"] T.IntT]
                    [
                        T.VarDef [["r"]] (T.ValueM [T.WordP "m", T.WordP "times", T.IntP 2]),
                        T.Result (T.ValueM [T.WordP "r"])
                    ]
                ),

        testCase "Consecutive arguments in title" $
            expectedFailure P.functionDefinition "The double of an integer (m) an integer (n):\n  The result is m times n."
    ]

titleTests :: TestTree
titleTests = testGroup "Title"
    [
        testCase "Words" $
            expectedResult
                P.title
                "Function definition"
                [T.TitleWords ["Function", "definition"]],

        testCase "Many parts" $
            expectedResult
                P.title
                "Definition with an integer (m) and an integer (n)"
                [
                    T.TitleWords ["Definition", "with"],
                    T.TitleParam ["m"] T.IntT,
                    T.TitleWords ["and"],
                    T.TitleParam ["n"] T.IntT
                ],

        testCase "Consecutive arguments" $
            expectedResult
                P.title
                "Definition with an integer (m) an integer (n)"
                [
                    T.TitleWords ["Definition", "with"],
                    T.TitleParam ["m"] T.IntT
                ],

        testCase "Missing name" $
            expectedFailure P.title "Definition with an integer"
    ]

titleWordsTests :: TestTree
titleWordsTests = testGroup "Title words"
    [
        testCase "Words" $
            expectedResult
                P.titleWords
                "Function definition"
                (T.TitleWords ["Function", "definition"]),

        testCase "Words followed by reserved word" $
            expectedResult
                P.titleWords
                "Function definition and"
                (T.TitleWords ["Function", "definition", "and"]),

        testCase "Words followed by parameter" $
            expectedResult
                P.titleWords
                "Function definition an integer"
                (T.TitleWords ["Function", "definition"]),

        testCase "Reserved word first" $
            expectedResult
                P.titleWords
                "And function definition"
                (T.TitleWords ["And", "function", "definition"])
    ]

titleParamTests :: TestTree
titleParamTests = testGroup "Title parameter"
    [
        testCase "Named" $
            expectedResult
                P.titleParam
                "An integer (m)"
                (T.TitleParam ["m"] T.IntT),

        testCase "Followed by words" $
            expectedResult
                P.titleParam
                "An integer (m) function definition"
                (T.TitleParam ["m"] T.IntT),

        testCase "Two parameters" $
            expectedResult
                P.titleParam
                "An integer (m) an integer (n)"
                (T.TitleParam ["m"] T.IntT),

        testCase "Missing name" $
            expectedFailure P.titleParam "An integer",

        testCase "Missing article" $
            expectedFailure P.titleParam "integer (m)"
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
                (P.listWithHeader P.anyWord integerItems)
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
        integerItems :: P.Parser (String, [Integer])
        integerItems = P.listWithHeader P.anyWord (P.integer <* P.dot)

valueTests :: TestTree
valueTests = testGroup "Value"
    [
         testCase "List" $
            expectedResult
                P.value
                "A list of integers containing a, b, and c"
                (T.ListV T.IntT [T.ValueM [T.WordP "a"], T.ValueM [T.WordP "b"], T.ValueM [T.WordP "c"]])
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
