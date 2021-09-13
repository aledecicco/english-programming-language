{-# LANGUAGE TupleSections #-}
{-|
Module      : FuzzyParser
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The language's first phase of parsing.
Builds an AST that may have unparsed Values ('ValueM') or Sentences ('SentenceM'), which are then transformed into concrete Values or Sentences by the "Solver".
-}

module FuzzyParser where

import Control.Monad (void)
import Control.Monad.Trans.State.Strict (get, put, runStateT, StateT)
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST
import Errors


-- -----------------
-- * Parser definition

-- | The fuzzy parser's monad.
-- An inner parser monad wrapped with a state that keeps track of a boolean.
-- The boolean determines whether the parser is at the beggining of a sentence, which is used for casing validations.
-- The state transforms the parser and not the other way around so that the state is rolled back when the parser fails.
type FuzzyParser = StateT Bool (Parsec Void String)

-- | Consumes whitespace including newlines.
consumeWhitespace :: FuzzyParser ()
consumeWhitespace = L.space space1 empty empty

-- | Consumes whitespace not including newlines.
consumeLineWhitespace :: FuzzyParser ()
consumeLineWhitespace = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- | Wrapper for parsing lexemes consuming trailing whitespace.
lexeme :: FuzzyParser a -> FuzzyParser a
lexeme = L.lexeme consumeLineWhitespace

-- | Parses a string consuming trailing whitespace.
symbol :: String -> FuzzyParser ()
symbol = void . L.symbol consumeLineWhitespace

-- | The list of words that can't be used in variable names.
-- These words being reserved makes it easier to parse 'IterV', 'VarDef' and 'ForEach'.
reservedWords :: [String]
reservedWords = ["be", "in"]


-- -----------------
-- * Auxiliary

-- | Parses a specific case sensitive word.
word :: String -> FuzzyParser String
word w = lexeme $ string w <* notFollowedBy alphaNumChar

-- | If the parser is at the beggining of a sentence, parses the word with its first letter in upper case.
-- Otherwise, parses the word as is.
firstWord :: String -> FuzzyParser String
firstWord w@(x:xs) = do
    isFirst <- get
    if isFirst
        then word (toUpper x : xs) <* put False
        else word w
firstWord "" = error "Can't parse an empty string"

ordinal :: FuzzyParser String
ordinal = do
    num <- some digitChar
    suffix <-
        case last num of
            '1' -> word "st"
            '2' -> word "nd"
            '3' -> word "rd"
            _ -> word "th"
    return $ num ++ suffix

-- | Parses any valid word in the language, including ordinals.
anyWord :: FuzzyParser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <|> try ordinal <?> "word"

-- | Parses any word and checks that it's not reserved.
identifier :: FuzzyParser String
identifier = do
    w <- anyWord
    if w `elem` reservedWords
        then fail $ "Incorrect use of reserved word \"" ++ w ++ "\""
        else return w

-- | Parses a name (list of words that are not reserved).
name :: FuzzyParser Name
name = (some . try) identifier <?> "name"

-- | Parses a type in singular or plural.
typeName ::
    Bool -- ^ Whether the parsed type is plural or not.
    -> FuzzyParser Type
typeName False =
    (word "whole" >> word "number" >> return IntT)
    <|> (word "number" >> return FloatT)
    <|> (word "boolean" >> return BoolT)
    <|> (word "character" >> return CharT)
    <|> (word "string" >> return (ListT CharT))
    <|> (do
            word "list"
            word "of"
            elemsType <- typeName True
            return $ ListT elemsType)
    <?> "singular type"
typeName True =
    (word "whole" >> word "numbers" >> return IntT)
    <|> (word "numbers" >> return FloatT)
    <|> (word "booleans" >> return BoolT)
    <|> (word "characters" >> return CharT)
    <|> (word "strings" >> return (ListT CharT))
    <|> (do
            word "lists"
            word "of"
            elemsType <- typeName True
            return $ ListT elemsType)
    <?> "plural type"

-- | Parses a type referencing another type.
referenceType :: FuzzyParser Type
referenceType = (do
    word "reference"
    word "to"
    word "a"
    RefT <$> typeName False)
    <?> "reference type"

integer :: FuzzyParser Int
integer = lexeme (L.signed (return ()) L.decimal <* notFollowedBy alphaNumChar) <?> "number"

float :: FuzzyParser Float
float = lexeme (L.signed (return ()) L.float <* notFollowedBy alphaNumChar) <?> "float"

-- | Parses a char between single quotes.
charLiteral :: FuzzyParser Char
charLiteral = lexeme (between (char '\'') (char '\'') L.charLiteral) <?> "char"

-- | Parses a string between double quotes.
stringLiteral :: FuzzyParser String
stringLiteral = lexeme (char '"' >> manyTill (notFollowedBy (char '\n' <|> char '\r') >> L.charLiteral) (char '"')) <?> "string"

comma :: FuzzyParser ()
comma = symbol "," <?> "comma"

dot :: FuzzyParser ()
dot = symbol "." <?> "dot"

colon :: FuzzyParser ()
colon = symbol ":" <?> "colon"

-- | Runs the given parser between parenthesis.
parens :: FuzzyParser a -> FuzzyParser a
parens = between (symbol "(") (symbol ")")

-- | Returns the current location in the source code.
getCurrentLocation :: FuzzyParser Location
getCurrentLocation = do
    pos <- getSourcePos
    return (unPos $ sourceLine pos, unPos $ sourceColumn pos)


-- -----------------
-- * Combinators

-- | Parses a series of a given parser.
-- If there is more than one element, there must be a comma between each one, and the word "and" before the last one.
--
-- >>> parseMaybe (series integer) "1, 2, and 3"
-- Just [1,2,3]
--
-- >>> parseMaybe (series integer) "1, 2 and 3"
-- Nothing
series :: FuzzyParser a -> FuzzyParser [a]
series p = do
    x <- p
    xs <- series' p <|> return []
    return $ x:xs
    where
        series' :: FuzzyParser a -> FuzzyParser [a]
        series' p = do
            comma
            xs <- (many . try) $ p <* comma
            word "and"
            x' <- p
            return $ xs++[x']

-- | Intercalates two parsers.
-- There must be at least one occurence of the first parser in order for this combinator to succeed.
--
-- >>> parseMaybe (intercalated (word "odd") (word "even")) "odd even odd even"
-- Just ["odd","even","odd","even"]
--
-- >>> parseMaybe (intercalated (word "odd") (word "even")) "odd even odd even odd"
-- Just ["odd","even","odd","even","odd"]
intercalated :: FuzzyParser a -> FuzzyParser a -> FuzzyParser [a]
intercalated pA pB = do
    a <- pA
    rest <- try (intercalated pB pA) <|> return []
    return $ a:rest

-- | Parses a header ending in a colon followed by a block of indented elements.
sentenceBlock :: FuzzyParser a -> FuzzyParser (a, [Annotated Sentence])
sentenceBlock pHeader = L.indentBlock consumeWhitespace sentenceBlock'
    where
        sentenceBlock' = do
            header <- try $ pHeader <* colon
            return $ L.IndentSome Nothing (return . (header, )) sentence


-- -----------------
-- * Values

value :: FuzzyParser (Annotated Value)
value = listValue <|> valueMatchable

-- | Parses a list with matchables as elements.
listValue :: FuzzyParser (Annotated Value)
listValue = do
    ann <- getCurrentLocation
    try $ word "a" >> word "list"
    word "of"
    elemsType <- typeName True
    elems <- (word "containing" >> series valueMatchable) <|> return []
    return $ ListV ann elemsType elems

valueMatchable :: FuzzyParser (Annotated Value)
valueMatchable = do
    ann <- getCurrentLocation
    parts <- some matchablePart
    return $ ValueM ann parts

-- | Parses the condition for a control statement.
condition :: FuzzyParser (Annotated Value)
condition = valueMatchable

matchablePart :: FuzzyParser (Annotated MatchablePart)
matchablePart = do
    ann <- getCurrentLocation
    try (FloatP ann <$> float)
        <|> IntP ann <$> integer
        <|> CharP ann <$> charLiteral
        <|> StringP ann <$> stringLiteral
        <|> WordP ann <$> anyWord
        <|> ParensP <$> parens (some matchablePart)
        <?> "valid term"


-- -----------------
-- * Sentences

-- | Parses a simple conditional sentence identified by the given word.
-- Returns the condition after the identifier and the simple sentence inside the conditional.
simpleConditional :: String -> FuzzyParser (Annotated Value, [Annotated Sentence])
simpleConditional idWord = do
    firstWord idWord
    cond <- valueMatchable
    comma
    s <- simpleSentence
    return (cond, [s])

-- | Parses a simple `when` statement, which works as an `if` without an `else` clause.
simpleWhen :: FuzzyParser (Annotated Sentence)
simpleWhen = do
    ann <- getCurrentLocation
    uncurry (When ann) <$> simpleConditional "when"

-- | Parses a simple `unless`, which works as a `when` with its condition negated.
simpleUnless :: FuzzyParser (Annotated Sentence)
simpleUnless = do
    ann <- getCurrentLocation
    uncurry (Unless ann) <$> simpleConditional "unless"

-- | Parses a simple `if-else` statement.
simpleIfElse :: FuzzyParser (Annotated Sentence)
simpleIfElse = do
    ann <- getCurrentLocation
    (cond, ssIf) <- simpleConditional "if"
    comma
    word "otherwise"
    sElse <- simpleSentence
    return $ IfElse ann cond ssIf [sElse]

-- | Parses a simple `while` loop.
simpleWhile :: FuzzyParser (Annotated Sentence)
simpleWhile = do
    ann <- getCurrentLocation
    uncurry (While ann) <$> simpleConditional "while"

-- | Parses a simple `until` loop, which works as a `while` with its condition negated.
simpleUntil :: FuzzyParser (Annotated Sentence)
simpleUntil = do
    ann <- getCurrentLocation
    uncurry (Until ann) <$> simpleConditional "until"

-- | The header of a `for-each` loop, which includes the name and type of the iterator, and the value to be iterated.
forEachHeader :: FuzzyParser (Name, Type, Annotated Value)
forEachHeader = do
    try $ firstWord "for" >> word "each"
    iterType <- typeName False
    iterName <- parens name
    word "in"
    listVal <- valueMatchable
    return (iterName, iterType, listVal)

-- | Parses a simple `for-each` loop.
simpleForEach :: FuzzyParser (Annotated Sentence)
simpleForEach = do
    ann <- getCurrentLocation
    (iterName, iterType, listVal) <- try $ forEachHeader <* comma
    s <- simpleSentence
    return $ ForEach ann iterName iterType listVal [s]

-- | Parses a simple `attempt` statement, which works as a `try` without a `catch` clause.
simpleAttempt :: FuzzyParser (Annotated Sentence)
simpleAttempt = do
    ann <- getCurrentLocation
    firstWord "attempt" >> word "to"
    s <- simpleSentence
    return $ Attempt ann [s]

-- | Parses a simple `try-catch` statement.
simpleTryCatch :: FuzzyParser (Annotated Sentence)
simpleTryCatch = do
    ann <- getCurrentLocation
    try $ firstWord "try" >> word "to"
    sTry <- simpleSentence
    comma
    mapM_ word ["and", "in", "case", "of", "error"]
    sCatch <- simpleSentence
    return $ TryCatch ann [sTry] [sCatch]

-- | Parses a return statement.
result :: FuzzyParser (Annotated Sentence)
result = do
    ann <- getCurrentLocation
    firstWord "return"
    Return ann <$> valueMatchable

-- | Parses a throw statement.
throw :: FuzzyParser (Annotated Sentence)
throw = do
    ann <- getCurrentLocation
    try $ firstWord "throw" >> word "an" >> word "error"
    word "because"
    Throw ann <$> some anyWord

-- | Parses a simple sentence, which can be any sentence in its simple form (except for `let` statements) or a sentence matchable.
-- Simple sentences can only contain other simple sentences.
simpleSentence :: FuzzyParser (Annotated Sentence)
simpleSentence =
    simpleWhen
    <|> simpleUnless
    <|> simpleIfElse
    <|> simpleUntil
    <|> simpleWhile
    <|> simpleForEach
    <|> simpleAttempt
    <|> simpleTryCatch
    <|> result
    <|> throw
    <|> sentenceMatchable
    <?> "simple sentence"

-- | Parses the definition of one or more variables with the same value.
variablesDefinition :: FuzzyParser (Annotated Sentence)
variablesDefinition = do
    defAnn <- getCurrentLocation
    firstWord "let"
    names <- series name
    word "be"
    valAnn <- getCurrentLocation
    varType <-
        (case names of
            -- If there is only one variable, ask for its type in singular.
            [_] -> try (word "a" >> Just <$> typeName False)
            -- If there are more, ask for their type in plural.
            _ -> try (Just <$> typeName True))
        <|> return Nothing
    val <- case varType of
        Just (ListT elemsType) ->
            -- List by extension.
            (do
               word "containing"
               elems <- series valueMatchable
               return $ ListV valAnn elemsType elems)
            -- List value.
            <|> (word "equal" >> word "to" >> valueMatchable)
            -- Empty list.
            <|> return (ListV valAnn elemsType [])
        -- Typed value.
        Just _ -> word "equal" >> word "to" >> valueMatchable
        -- Untyped value.
        Nothing -> valueMatchable
    return $ VarDef defAnn names varType val

-- | Parses a block `when` statement, which works as an `if` without an `else` clause.
blockWhen :: FuzzyParser (Annotated Sentence)
blockWhen = do
    ann <- getCurrentLocation
    (cond, ss) <- sentenceBlock $ firstWord "when" >> valueMatchable
    return $ When ann cond ss

-- | Parses a block `unless` statement, which works as a `when` with its condition negated.
blockUnless :: FuzzyParser (Annotated Sentence)
blockUnless = do
    ann <- getCurrentLocation
    (cond, ss) <- sentenceBlock $ firstWord "unless" >> valueMatchable
    return $ Unless ann cond ss

-- | Parses a block `if-else` statement.
blockIfElse :: FuzzyParser (Annotated Sentence)
blockIfElse = do
    ann <- getCurrentLocation
    (cond, ssTrue) <- sentenceBlock $ firstWord "if" >> valueMatchable
    put True
    (_, ssFalse) <- sentenceBlock $ firstWord "otherwise"
    return $ IfElse ann cond ssTrue ssFalse

-- | Parses a block `while` loop.
blockUntil :: FuzzyParser (Annotated Sentence)
blockUntil = do
    ann <- getCurrentLocation
    (cond, ss) <- sentenceBlock $ firstWord "until" >> valueMatchable
    return $ Until ann cond ss

-- | Parses a block `until` loop, which works as a `while` with its condition negated.
blockWhile :: FuzzyParser (Annotated Sentence)
blockWhile = do
    ann <- getCurrentLocation
    (cond, ss) <- sentenceBlock $ firstWord "while" >> valueMatchable
    return $ While ann cond ss

-- | Parses a block `for-each` loop.
blockForEach :: FuzzyParser (Annotated Sentence)
blockForEach = do
    ann <- getCurrentLocation
    ((iterName, iterType, listVal), ss) <- sentenceBlock forEachHeader
    return $ ForEach ann iterName iterType listVal ss

-- | Parses a block `attempt` statement, which works as a `try` without a `catch` clause.
blockAttempt :: FuzzyParser (Annotated Sentence)
blockAttempt = do
    ann <- getCurrentLocation
    (_, ss) <- sentenceBlock $ firstWord "attempt" >> word "to"
    return $ Attempt ann ss

-- | Parses a block `try-catch` statement.
blockTryCatch :: FuzzyParser (Annotated Sentence)
blockTryCatch = do
    ann <- getCurrentLocation
    (_, ssTry) <- sentenceBlock $ firstWord "try" >> word "to"
    put True
    (_, ssCatch) <- sentenceBlock $ firstWord "in" >> mapM word ["case", "of", "error"]
    return $ TryCatch ann ssTry ssCatch

-- | Parses a block sentence.
blockSentence :: FuzzyParser (Annotated Sentence)
blockSentence =
    (variablesDefinition <* dot)
    <|> blockWhen
    <|> blockUnless
    <|> blockIfElse
    <|> blockUntil
    <|> blockWhile
    <|> blockForEach
    <|> blockAttempt
    <|> blockTryCatch
    <?> "block sentence"

-- | Parses a sentence that must be understood by the "Solver".
sentenceMatchable :: FuzzyParser (Annotated Sentence)
sentenceMatchable = do
    ann <- getCurrentLocation
    parts <- some matchablePart
    return $ SentenceM ann parts

-- | Parses a top-level sentence, starting with an upper case letter and ending in a stop.
-- Can be any simple or block sentence.
sentence :: FuzzyParser (Annotated Sentence)
sentence = put True >> (blockSentence <|> (simpleSentence <* dot) <?> "sentence")


-- -----------------
-- * Definitions

-- | Parses the first part of a function definition, which states the type of function it will be.
returnType :: FuzzyParser (Maybe Type)
returnType =
    -- Shorthand for predicates.
    (word "Whether" >> return (Just BoolT))
    -- Procedures
    <|> (word "To" >> notFollowedBy (word "a") >> lookAhead lowerChar >> return Nothing)
    -- Any kind of operator
    <|> do
        word "A"
        retType <- typeName False
        word "equal"
        word "to"
        return $ Just retType
    <?> "return type"

-- | Parses words for an identifying part of a function's title.
titleWords :: FuzzyParser (Annotated TitlePart)
titleWords = do
    ann <- getCurrentLocation
    words <- some (notFollowedBy (word "a") >> anyWord)
    return $ TitleWords ann words

-- | Parses a parameter of a function's title.
titleParam :: FuzzyParser (Annotated TitlePart)
titleParam = do
    ann <- getCurrentLocation
    word "a"
    pType <- referenceType <|> typeName False
    -- Parameters can optionally be named.
    pNames <- ((:[]) <$> parens name) <|> return []
    return $ TitleParam ann pNames pType

title :: FuzzyParser (Annotated Title)
title = do
    ann <- getCurrentLocation
    firstPart <- titleParam <|> titleWords
    rest <- case firstPart of
        (TitleWords _ _) -> intercalated titleParam titleWords <|> return []
        _ -> intercalated titleWords titleParam -- Titles must have at least one occurence of 'titleWords'.
    return $ Title ann (firstPart:rest)

functionDefinition :: FuzzyParser (Annotated Definition)
functionDefinition = do
    ann <- getCurrentLocation
    ((funTitle, retType), ss) <- sentenceBlock functionHeader
    return $ FunDef ann funTitle retType ss
    where
        functionHeader :: FuzzyParser (Annotated Title, Maybe Type)
        functionHeader = do
            lookAhead upperChar
            retType <- returnType
            funTitle <- title
            return (funTitle, retType)

definition :: FuzzyParser (Annotated Definition)
definition = functionDefinition

-- -----------------
-- * Main

-- | Runs the given parser on a string and returns its result or the error it yielded.
runFuzzyParser ::
    FuzzyParser a -- ^ The parser to run.
    -> Bool -- ^ Whether the parsing will happen at the beggining of a sentence.
    -> String -- ^ The string to parse.
    -> Either Error a
runFuzzyParser parser isBegg str =
    case runParser (runStateT parser isBegg) "" str of
        Left err -> Left $ transformError err
        Right (res, _) -> Right res
    where
        -- Turn the inner parser's error into the language's custom error type.
        transformError :: ParseErrorBundle String Void -> Error
        transformError (ParseErrorBundle (err :| _) pState) =
            let msg = lines $ parseErrorTextPretty err
                -- Turn the lines of the error into sentences.
                msg' = intercalate ". " $ map (\(c:cs) -> toUpper c : cs) msg
                -- Move the position of the inner parser to the position of the error.
                (_, pState') = reachOffset (errorOffset err) pState
                -- Fetch the position of the inner parser.
                sPos = pstateSourcePos pState'
                pos = (unPos $ sourceLine sPos, unPos $ sourceColumn sPos)
            in Error (Just pos) (ParseError msg')

-- | Parses a program from the given file contents.
parseProgram :: String -> Either Error Program
parseProgram = runFuzzyParser parseProgram' False
    where
        parseProgram' :: FuzzyParser Program
        parseProgram' = some definition <* eof
