{-# LANGUAGE TupleSections #-}

module FuzzyParser where

import Data.Void ( Void )
import Data.Char ( toUpper )
import Control.Monad ( void )

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

--


-- Parser definitions

type FuzzyParser = Parsec Void String
type Error = ParseErrorBundle String Void

-- Parses whitespace including new lines
scn :: FuzzyParser ()
scn = L.space space1 empty empty

-- Consumes whitespace not including new lines
sc :: FuzzyParser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- Wrapper for parsing lexemes consumig trailing whitespace
lexeme :: FuzzyParser a -> FuzzyParser a
lexeme = L.lexeme sc

-- Parses a string consuming trailing whitespace
symbol :: String -> FuzzyParser ()
symbol = void . L.symbol sc

reservedWords :: [String]
reservedWords = ["be", "in"]

--


-- Auxiliary

-- Parses a name (list of words that are not reserved)
name :: FuzzyParser Name
name = (some . try) identifier <?> "name"

-- Parses a type by its name
baseType :: Bool -> FuzzyParser Type
baseType False =
    (word "whole" >> word "number" >> return IntT)
    <|> (word "number" >> return FloatT)
    <|> (word "boolean" >> return BoolT)
    <|> (word "character" >> return CharT)
    <|> (word "string" >> return (ListT CharT))
    <|> (do
            word "list"
            word "of"
            eT <- baseType True
            return $ ListT eT)
    <?> "singular type"
baseType True =
    (word "whole" >> word "numbers" >> return IntT)
    <|> (word "numbers" >> return FloatT)
    <|> (word "booleans" >> return BoolT)
    <|> (word "characters" >> return CharT)
    <|> (word "strings" >> return (ListT CharT))
    <|> (do
            word "lists"
            word "of"
            eT <- baseType True
            return $ ListT eT)
    <?> "plural type"

referenceType :: FuzzyParser Type
referenceType = (do
    word "reference"
    word "to"
    word "a"
    RefT <$> baseType False)
    <?> "reference type"


-- Parses any word and checks that it's not reserved
identifier :: FuzzyParser String
identifier = do
    w <- anyWord
    if w `elem` reservedWords
        then fail $ "Incorrect use of reserved word \"" ++ w ++ "\""
        else return w

-- Parses a specific word
word :: String -> FuzzyParser String
word w = lexeme $ string w <* notFollowedBy alphaNumChar

-- Parses a keyword that can have its first letter in uppercase
firstWord :: String -> FuzzyParser String
firstWord w@(x:xs) = word w <|> word (toUpper x : xs)
firstWord "" = error "Can't parse an empty string"

anyWord :: FuzzyParser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <?> "word"

integer :: FuzzyParser Int
integer = lexeme (L.signed (return ()) L.decimal <* notFollowedBy alphaNumChar) <?> "number"

float :: FuzzyParser Float
float = lexeme (L.signed (return ()) L.float <* notFollowedBy alphaNumChar) <?> "float"

charLiteral :: FuzzyParser Char
charLiteral = lexeme (char '\'' >> L.charLiteral <* char '\'') <?> "char"

stringLiteral :: FuzzyParser String
stringLiteral = lexeme (char '"' >> manyTill L.charLiteral (char '"')) <?> "string"

comma :: FuzzyParser ()
comma = symbol "," <?> "comma"

dot :: FuzzyParser ()
dot = symbol "." <?> "dot"

colon :: FuzzyParser ()
colon = symbol ":" <?> "colon"

parens :: FuzzyParser a -> FuzzyParser a
parens = between (symbol "(") (symbol ")")

getCurrentLocation :: FuzzyParser Location
getCurrentLocation = do
    p <- getSourcePos
    return (unPos $ sourceLine p, unPos $ sourceColumn p)

--


-- Combinators

-- Parses a series of a given parser (such as "a, b and c")
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

-- Parses an intercalated list of two parsers
intercalated :: FuzzyParser a -> FuzzyParser a -> FuzzyParser [a]
intercalated pA pB = do
    a <- pA
    r <- try (intercalated pB pA) <|> return []
    return $ a:r

-- Parses a header followed by a block of indented elements
listWithHeader :: FuzzyParser a -> FuzzyParser b -> FuzzyParser (a, [b])
listWithHeader pH pE = L.indentBlock scn listWithHeader'
    where
        listWithHeader' = do
            h <- pH
            colon
            return $ L.IndentSome Nothing (return . (h, )) pE

-- Parses a specific sentence in block format
blockSentence :: FuzzyParser a -> FuzzyParser (a, [Annotated Sentence])
blockSentence pH = do
    (h, ss) <- listWithHeader pH sentence
    return (h, ss)

--


-- Blocks

block :: FuzzyParser (Annotated Block)
block = functionDefinition

functionDefinition :: FuzzyParser (Annotated Block)
functionDefinition = do
    ann <- getCurrentLocation
    ((t, rt), ss) <- listWithHeader functionHeader sentence
    return $ FunDef ann t rt ss
    where
        functionHeader :: FuzzyParser (Annotated Title, Maybe Type)
        functionHeader = do
            rt <- returnType
            tSource <- title
            return (tSource, rt)

returnType :: FuzzyParser (Maybe Type)
returnType =
    (word "Whether" >> return (Just BoolT))
    <|> (word "To" >> return Nothing)
    <|> do
        word "A"
        rt <- baseType False
        word "equal"
        word "to"
        return $ Just rt
    <?> "return type"

title :: FuzzyParser (Annotated Title)
title = do
    ann <- getCurrentLocation
    x <- titleParam True <|> titleWords
    xs <- case x of
        (TitleWords _ _) -> intercalated (titleParam False) titleWords <|> return []
        _ -> intercalated titleWords (titleParam False)
    return $ Title ann (x:xs)

-- Parses words for an identifying part of a function's title
titleWords :: FuzzyParser (Annotated TitlePart)
titleWords = do
    ann <- getCurrentLocation
    ws <- some (notFollowedBy (word "a") >> anyWord)
    return $ TitleWords ann ws

-- Parses a parameter of a function's title
titleParam :: Bool -> FuzzyParser (Annotated TitlePart)
titleParam isFirst = do
    ann <- getCurrentLocation
    if isFirst then firstWord "a" else word "a"
    t <- referenceType <|> baseType False
    a <- parens name <?> "parameter name"
    return $ TitleParam ann a t

--


-- Sentences

sentence :: FuzzyParser (Annotated Sentence)
sentence = lookAhead upperChar >> do
        variablesDefinition <* dot
        <|> (simpleIf <* dot)
        <|> ifBlock
        <|> (simpleForEach <* dot)
        <|> forEachBlock
        <|> (simpleUntil <* dot)
        <|> untilBlock
        <|> (simpleWhile <* dot)
        <|> whileBlock
        <|> (result <* dot)
        <|> (sentenceMatchable <* dot)
        <?> "sentence"

-- Parses a sentence that can be used inside a simple statement
simpleSentence :: FuzzyParser (Annotated Sentence)
simpleSentence = variablesDefinition <|> result <|> sentenceMatchable <?> "simple sentence"

-- Parses the definition of one or more variables with the same value
variablesDefinition :: FuzzyParser (Annotated Sentence)
variablesDefinition = do
    ann <- getCurrentLocation
    firstWord "let"
    ns <- series name
    word "be"
    ann' <- getCurrentLocation
    t <-
        (case ns of
            [_] -> try (word "a" >> Just <$> baseType False)
            _ -> try (Just <$> baseType True))
        <|> return Nothing
    let listElems eT = do
            l <- word "containing" >> series valueMatchable
            return $ ListV ann' eT l
        emptyList eT = return $ ListV ann' eT []
        typedVal = do
            word "equal"
            word "to"
            valueMatchable
        untypedVal = valueMatchable
    v <- case t of
        Just (ListT eT) ->
           listElems eT <|> typedVal <|> emptyList eT
        Just _ -> typedVal
        Nothing -> untypedVal
    return $ VarDef ann ns t v

conditionalHeader :: String -> FuzzyParser (Annotated Value)
conditionalHeader w = firstWord w >> condition

simpleIf :: FuzzyParser (Annotated Sentence)
simpleIf = do
    ann <- getCurrentLocation
    c <- try $ conditionalHeader "if" <* comma
    s <- simpleSentence
    (IfElse ann c [s] <$> simpleElse) <|> return (If ann c [s])
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
    (c, ss) <- blockSentence $ conditionalHeader "if"
    (IfElse ann c ss <$> elseBlock) <|> return (If ann c ss)
    where
        elseBlock :: FuzzyParser [Annotated Sentence]
        elseBlock = snd <$> listWithHeader (word "Otherwise") sentence

simpleUntil :: FuzzyParser (Annotated Sentence)
simpleUntil = do
    ann <- getCurrentLocation
    c <- try $ conditionalHeader "until" <* comma
    s <- simpleSentence
    return $ Until ann c [s]

untilBlock :: FuzzyParser (Annotated Sentence)
untilBlock = do
    ann <- getCurrentLocation
    (c, ss) <- blockSentence $ conditionalHeader "until"
    return $ Until ann c ss

simpleWhile :: FuzzyParser (Annotated Sentence)
simpleWhile = do
    ann <- getCurrentLocation
    c <- try $ conditionalHeader "while" <* comma
    s <- simpleSentence
    return $ While ann c [s]

whileBlock :: FuzzyParser (Annotated Sentence)
whileBlock = do
    ann <- getCurrentLocation
    (c, ss) <- blockSentence $ conditionalHeader "while"
    return $ While ann c ss

forEachHeader :: FuzzyParser (Name, Type, Annotated Value)
forEachHeader = do
    firstWord "for"
    word "each"
    t <- baseType False
    n <- parens name
    word "in"
    l <- valueMatchable
    return (n, t, l)

simpleForEach :: FuzzyParser (Annotated Sentence)
simpleForEach = do
    ann <- getCurrentLocation
    (n, t, l) <- try $ forEachHeader <* comma
    s <- simpleSentence
    return $ ForEach ann n t l [s]

forEachBlock :: FuzzyParser (Annotated Sentence)
forEachBlock = do
    ann <- getCurrentLocation
    ((n, t, l), ss) <- blockSentence forEachHeader
    return $ ForEach ann n t l ss

-- Parses a return statement
result :: FuzzyParser (Annotated Sentence)
result = do
    ann <- getCurrentLocation
    try $ firstWord "the" >> word "result"
    word "is"
    Result ann <$> value

sentenceMatchable :: FuzzyParser (Annotated Sentence)
sentenceMatchable = do
    ann <- getCurrentLocation
    ps <- some matchablePart
    return $ SentenceM ann ps

--


-- Values

value :: FuzzyParser (Annotated Value)
value = listValue <|> valueMatchable

-- Parses a list with matchables as elements
listValue :: FuzzyParser (Annotated Value)
listValue = do
    ann <- getCurrentLocation
    try $ word "a" >> word "list"
    word "of"
    t <- baseType True
    l <- (word "containing" >> series valueMatchable) <|> return []
    return $ ListV ann t l

valueMatchable :: FuzzyParser (Annotated Value)
valueMatchable = do
    ann <- getCurrentLocation
    ps <- some matchablePart
    return $ ValueM ann ps

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

--


-- Main

-- Parses a string using a specific parser and returns its result or the error it yielded
runFuzzyParser :: FuzzyParser a -> String -> Either String a
runFuzzyParser p s =
    case parse p "" s of
        Left e -> Left $ errorBundlePretty e
        Right a -> Right a

parseProgram :: String -> Either String Program
parseProgram = runFuzzyParser parseProgram'
    where
        parseProgram' :: FuzzyParser Program
        parseProgram' = some block <* eof

--
