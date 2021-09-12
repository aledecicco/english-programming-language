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
type FuzzyParser = Parsec Void String

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

-- | Parses a keyword that can have its first letter in uppercase.
firstWord :: String -> FuzzyParser String
firstWord w@(x:xs) = word w <|> word (toUpper x : xs)
firstWord "" = error "Can't parse an empty string"

-- | Parses any valid word in the language, including ordinals.
anyWord :: FuzzyParser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <|> ordinal <?> "word"
    where
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
listWithHeader :: FuzzyParser a -> FuzzyParser b -> FuzzyParser (a, [b])
listWithHeader pHeader pBody = L.indentBlock consumeWhitespace listWithHeader'
    where
        listWithHeader' = do
            header <- pHeader
            colon
            return $ L.IndentSome Nothing (return . (header, )) pBody

-- | Parses the header and body of a sentence in block format.
blockSentence :: FuzzyParser a -> FuzzyParser (a, [Annotated Sentence])
blockSentence pHeader = do
    (header, ss) <- listWithHeader pHeader sentence
    return (header, ss)


-- -----------------
-- * Blocks

block :: FuzzyParser (Annotated Block)
block = functionDefinition

functionDefinition :: FuzzyParser (Annotated Block)
functionDefinition = do
    ann <- getCurrentLocation
    ((funTitle, retType), ss) <- listWithHeader functionHeader sentence
    return $ FunDef ann funTitle retType ss
    where
        functionHeader :: FuzzyParser (Annotated Title, Maybe Type)
        functionHeader = do
            lookAhead upperChar
            retType <- returnType
            funTitle <- title
            return (funTitle, retType)

-- | Parses the first part of a function definition, which states the type of function it will be.
returnType :: FuzzyParser (Maybe Type)
returnType =
    -- Shorthand for predicates.
    (firstWord "whether" >> return (Just BoolT))
    -- Procedures
    <|> (firstWord "to" >> notFollowedBy (word "a") >> return Nothing)
    -- Any kind of operator
    <|> do
        firstWord "a"
        retType <- typeName False
        word "equal"
        word "to"
        return $ Just retType
    <?> "return type"

title :: FuzzyParser (Annotated Title)
title = do
    ann <- getCurrentLocation
    firstPart <- titleParam <|> titleWords
    rest <- case firstPart of
        (TitleWords _ _) -> intercalated titleParam titleWords <|> return []
        _ -> intercalated titleWords titleParam -- Titles must have at least one occurence of 'titleWords'.
    return $ Title ann (firstPart:rest)

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


-- -----------------
-- * Sentences

-- | Parses a top-level sentence, starting with an uppercase letter and ending in a stop.
sentence :: FuzzyParser (Annotated Sentence)
sentence = lookAhead upperChar >>
    ((variablesDefinition <* dot)
    <|> (simpleIf <* dot)
    <|> ifBlock
    <|> (simpleForEach <* dot)
    <|> forEachBlock
    <|> (simpleUntil <* dot)
    <|> untilBlock
    <|> (simpleWhile <* dot)
    <|> whileBlock
    <|> (result <* dot)
    <|> throw
    <|> (simpleTry <* dot)
    <|> tryBlock
    <|> (sentenceMatchable <* dot)
    <?> "sentence")

-- | Parses a sentence that can be used inside a simple statement.
simpleSentence :: FuzzyParser (Annotated Sentence)
simpleSentence = variablesDefinition <|> result <|> throw <|> sentenceMatchable <?> "simple sentence"

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
            -- If there are more than one variables, ask for their type in plural.
            _ -> try (Just <$> typeName True))
        <|> return Nothing
    val <- case varType of
        Just (ListT elemsType) ->
            -- List with elements.
            (do
               word "containing"
               elems <- series valueMatchable
               return $ ListV valAnn elemsType elems)
            -- List from value.
            <|> (word "equal" >> word "to" >> valueMatchable)
            -- Empty list.
            <|> return (ListV valAnn elemsType [])
        -- Typed value.
        Just _ -> word "equal" >> word "to" >> valueMatchable
        -- Untyped value.
        Nothing -> valueMatchable
    return $ VarDef defAnn names varType val

-- | The header of a conditional control statement.
conditionalHeader :: String -> FuzzyParser (Annotated Value)
conditionalHeader w = firstWord w >> condition

simpleIf :: FuzzyParser (Annotated Sentence)
simpleIf = do
    ann <- getCurrentLocation
    cond <- try $ conditionalHeader "if" <* comma
    s <- simpleSentence
    (IfElse ann cond [s] <$> simpleElse) <|> return (If ann cond [s])
    where
        simpleElse :: FuzzyParser [Annotated Sentence]
        simpleElse = do
            comma
            word "otherwise"
            s <- simpleSentence
            return [s]

ifBlock :: FuzzyParser (Annotated Sentence)
ifBlock = do
    ann <- getCurrentLocation
    (cond, ss) <- blockSentence $ conditionalHeader "if"
    (IfElse ann cond ss <$> elseBlock) <|> return (If ann cond ss)
    where
        elseBlock :: FuzzyParser [Annotated Sentence]
        elseBlock = snd <$> listWithHeader (word "Otherwise") sentence

simpleUntil :: FuzzyParser (Annotated Sentence)
simpleUntil = do
    ann <- getCurrentLocation
    cond <- try $ conditionalHeader "until" <* comma
    s <- simpleSentence
    return $ Until ann cond [s]

untilBlock :: FuzzyParser (Annotated Sentence)
untilBlock = do
    ann <- getCurrentLocation
    (cond, ss) <- blockSentence $ conditionalHeader "until"
    return $ Until ann cond ss

simpleWhile :: FuzzyParser (Annotated Sentence)
simpleWhile = do
    ann <- getCurrentLocation
    cond <- try $ conditionalHeader "while" <* comma
    s <- simpleSentence
    return $ While ann cond [s]

whileBlock :: FuzzyParser (Annotated Sentence)
whileBlock = do
    ann <- getCurrentLocation
    (cond, ss) <- blockSentence $ conditionalHeader "while"
    return $ While ann cond ss

forEachHeader :: FuzzyParser (Name, Type, Annotated Value)
forEachHeader = do
    firstWord "for"
    word "each"
    iterType <- typeName False
    iterName <- parens name
    word "in"
    listVal <- valueMatchable
    return (iterName, iterType, listVal)

simpleForEach :: FuzzyParser (Annotated Sentence)
simpleForEach = do
    ann <- getCurrentLocation
    (iterName, iterType, listVal) <- try $ forEachHeader <* comma
    s <- simpleSentence
    return $ ForEach ann iterName iterType listVal [s]

forEachBlock :: FuzzyParser (Annotated Sentence)
forEachBlock = do
    ann <- getCurrentLocation
    ((iterName, iterType, listVal), ss) <- blockSentence forEachHeader
    return $ ForEach ann iterName iterType listVal ss

-- | Parses a return statement.
result :: FuzzyParser (Annotated Sentence)
result = do
    ann <- getCurrentLocation
    firstWord "return"
    Return ann <$> value

tryHeader :: FuzzyParser ()
tryHeader = void $ firstWord "try" >> word "to"

simpleTry :: FuzzyParser (Annotated Sentence)
simpleTry = do
    ann <- getCurrentLocation
    s <- try $ tryHeader >> simpleSentence
    (TryCatch ann [s] <$> simpleCatch) <|> return (Try ann [s])
    where
        simpleCatch :: FuzzyParser [Annotated Sentence]
        simpleCatch = do
            comma
            word "and"
            word "in"
            word "case"
            word "of"
            word "error"
            s <- simpleSentence
            return [s]

tryBlock :: FuzzyParser (Annotated Sentence)
tryBlock = do
    ann <- getCurrentLocation
    (_, ss) <- blockSentence tryHeader
    (TryCatch ann ss <$> catchBlock) <|> return (Try ann ss)
    where
        catchBlock :: FuzzyParser [Annotated Sentence]
        catchBlock = snd <$> listWithHeader (word "In" >> word "case" >> word "of" >> word "error") sentence

throw :: FuzzyParser (Annotated Sentence)
throw = do
    ann <- getCurrentLocation
    try $ firstWord "throw" >> word "an" >> word "error"
    word "because"
    Throw ann <$> some anyWord

sentenceMatchable :: FuzzyParser (Annotated Sentence)
sentenceMatchable = do
    ann <- getCurrentLocation
    parts <- some matchablePart
    return $ SentenceM ann parts


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
-- * Main

-- | Runs the given parser on a string returns its result or the error it yielded.
runFuzzyParser :: FuzzyParser a -> String -> Either Error a
runFuzzyParser p str =
    case parse p "" str of
        Left (ParseErrorBundle (err :| _) pState) ->
            -- Turn megaparsec's error into the language's custom error type.
            let msg = lines $ parseErrorTextPretty err
                msg' = intercalate ". " $ map (\(c:cs) -> toUpper c : cs) msg
                (_, pState') = reachOffset (errorOffset err) pState
                sPos = pstateSourcePos pState'
                pos = (unPos $ sourceLine sPos, unPos $ sourceColumn sPos)
            in Left $ Error (Just pos) (ParseError msg')
        Right res -> Right res

-- | Parses a program from the given file contents.
parseProgram :: String -> Either Error Program
parseProgram = runFuzzyParser parseProgram'
    where
        parseProgram' :: FuzzyParser Program
        parseProgram' = some block <* eof
