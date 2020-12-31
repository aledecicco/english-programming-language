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

-- Asserts that a parser yields an error when parsing a given string
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

        testCase "Word" $
            expectedFailure P.integer "word",

        testCase "Followed by letter" $
            expectedFailure P.integer "word1"
    ]

stringLiteralTests :: TestTree
stringLiteralTests = testGroup "String literal"
    [
        testCase "Single word" $
            expectedResult
                P.stringLiteral
                "\"word\""
                "word",

        testCase "Reserved word" $
            expectedResult
                P.stringLiteral
                "\"and\""
                "and",

        testCase "Many chars" $
            expectedResult
                P.stringLiteral
                "\".,: and 123\""
                ".,: and 123",

        testCase "Escaped quote" $
            expectedResult
                P.stringLiteral
                "\"word \\\"another\\\" word\""
                "word \"another\" word",

        testCase "Not escaped quote" $
            expectedResult
                P.stringLiteral
                "\"word \"another\\\" word\""
                "word ",

        testCase "Without quotes" $
            expectedFailure P.stringLiteral "word",

        testCase "Not closed" $
            expectedFailure P.stringLiteral "\"word"
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
                "11 and 12"
                [11, 12],

        testCase "Three numbers" $
            expectedResult
                (P.series P.integer)
                "11, 12 and 13"
                [11, 12, 13],


        testCase "Without comma" $
            expectedResult
                (P.series P.integer)
                "11 12 and 13"
                [11],

        testCase "Without and" $
            expectedFailure (P.series P.integer) "11, 12, 13",

        testCase "Not and" $
            expectedFailure (P.series P.integer) "11, 12 android 13"
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

structDefinitionTests :: TestTree
structDefinitionTests = testGroup "Struct definition"
    [
        testCase "One field" $
            expectedResult
                P.structDefinition
                "Definition of car (cars):\n  A license plate (string)."
                (T.StructDef ["car"] ["cars"] [(["license", "plate"], T.TypeM ["string"])]),

        testCase "Many fields" $
            expectedResult
                P.structDefinition
                "Definition of car (cars):\n  A license plate (string).\n  A model year (number)."
                (T.StructDef ["car"] ["cars"] [(["license", "plate"], T.TypeM ["string"]), (["model", "year"], T.TypeM ["number"])]),

        testCase "Without plural" $
            expectedFailure P.structDefinition "Definition of car:\n  A license plate (string).\n  A model year (number)."
    ]

functionDefinitionTests :: TestTree
functionDefinitionTests = testGroup "Function definition"
    [
        testCase "Many sentences" $
            expectedResult
                P.functionDefinition
                "The double of a number (m):\n  Let r be m times 2.\n  The resulting number is r."
                (T.FunDef
                    [T.TitleWords ["The", "double", "of"], T.NamedTitleParamM ["m"] (T.TypeM ["number"])]
                    [
                        T.VarDef [["r"]] (T.ValueM [T.WordP "m", T.WordP "times", T.IntP 2]),
                        T.Result (T.TypeM ["number"]) (T.ValueM [T.WordP "r"])
                    ]
                ),

        testCase "Incorrect title" $
            expectedFailure P.functionDefinition "The double of a number (m) a number (n):\n  The resulting number is m times n."
    ]

titleTests :: TestTree
titleTests = testGroup "Title"
    [
        testCase "Words" $
            expectedResult
                P.title
                "Function definition"
                [T.TitleWords ["Function", "definition"]],

        testCase "Consecutive arguments" $
            expectedResult
                P.title
                "Definition with a number (m) a number (n)"
                [T.TitleWords ["Definition", "with"], T.NamedTitleParamM ["m"] (T.TypeM ["number"])],

        testCase "Many parts" $
            expectedResult
                P.title
                "Definition with a number (m) and a number (n)"
                [
                    T.TitleWords ["Definition", "with"],
                    T.NamedTitleParamM ["m"] (T.TypeM ["number"]),
                    T.TitleWords ["and"],
                    T.NamedTitleParamM ["n"] (T.TypeM ["number"])
                ],

        testCase "Argument" $
            expectedFailure P.title "A number"
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
                "Function definition a number"
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
        testCase "Unnamed" $
            expectedResult
                P.titleParam
                "A number"
                (T.UnnamedTitleParamM ["number"]),

        testCase "Unnamed followed by and" $
            expectedResult
                P.titleParam
                "A number and"
                (T.UnnamedTitleParamM ["number"]),

        testCase "Named" $
            expectedResult
                P.titleParam
                "A number (m)"
                (T.NamedTitleParamM ["m"] (T.TypeM ["number"])),

        testCase "Named followed by words" $
            expectedResult
                P.titleParam
                "A number (m) function definition"
                (T.NamedTitleParamM ["m"] (T.TypeM ["number"])),

        testCase "Two parameters" $
            expectedResult
                P.titleParam
                "A number (m) a number (n)"
                (T.NamedTitleParamM ["m"] (T.TypeM ["number"])),

        testCase "Unnamed with wrong end" $
            expectedResult
                P.titleParam
                "A number plus a number"
                (T.UnnamedTitleParamM ["number", "plus"]),

        testCase "No article" $
            expectedFailure P.titleParam "number (m)"
    ]

listWithHeaderTests :: TestTree
listWithHeaderTests = testGroup "ListWithHeader"
    [
         testCase "One element" $
            expectedResult
                numberItems
                "Numbers:\n  11."
                ("Numbers", [11]),

        testCase "Many elements" $
            expectedResult
                numberItems
                "Numbers:\n  11.\n  12."
                ("Numbers", [11, 12]),

        testCase "Second element not indented" $
            expectedResult
                numberItems
                "Numbers:\n  11.\n12."
                ("Numbers", [11]),

        testCase "Nested lists" $
            expectedResult
                (P.listWithHeader P.anyWord numberItems)
                "Lists:\n  First:\n    11.\n    12.\n  Second:\n    13.\n    14."
                ("Lists", [("First", [11, 12]), ("Second", [13, 14])]),

        testCase "No elements" $
            expectedFailure numberItems "Numbers:\n",

        testCase "Incorrect header" $
            expectedFailure numberItems "Number11:\n  11.",

        testCase "First element not indented" $
            expectedFailure numberItems "Numbers:\n11.",

        testCase "First element without dot" $
            expectedFailure numberItems "Numbers:\n  11",

        testCase "Second element without dot" $
            expectedFailure numberItems "Numbers:\n  11.\n  12"
    ]
    where
        numberItems :: P.Parser (String, [Integer])
        numberItems = P.listWithHeader P.anyWord (P.integer <* P.dot)
--


-- Main

tests :: TestTree
tests = testGroup "Parser"
    [
        anyWordTests,
        nameTests,
        identifierTests,
        reservedTests,
        integerTests,
        stringLiteralTests,
        parensTests,
        seriesTests,
        intercalatedTests,
        structDefinitionTests,
        functionDefinitionTests,
        titleTests,
        titleWordsTests,
        titleParamTests,
        listWithHeaderTests
    ]
