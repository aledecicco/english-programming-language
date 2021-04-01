{-# LANGUAGE TupleSections #-}

module FuzzyParser where

import Data.Void (Void)
import Data.Char ( toUpper )
import Control.Monad (void, when)

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
reservedWords = ["be", "in", "is", "an", "a", "containing"]

--


-- Auxiliary

firstWord :: FuzzyParser String
firstWord = lexeme (do
    l <- upperChar
    ls <- some letterChar
    notFollowedBy alphaNumChar
    return $ l:ls) <?> "first word"

anyWord :: FuzzyParser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <?> "word"

-- Parses a name (list of words that are not reserved)
name :: FuzzyParser Name
name = (some . try) identifier <?> "name"

-- Parses a type by its name
typeName :: Bool -> FuzzyParser Type
typeName True =
    (word "integers" >> return IntT)
    <|> (word "floats" >> return FloatT)
    <|> (word "booleans" >> return BoolT)
    <|> (do
            word "lists"
            word "of"
            eT <- typeName True
            return $ ListT eT)
    <?> "plural type"
typeName False =
    (word "integer" >> return IntT)
    <|> (word "float" >> return FloatT)
    <|> (word "boolean" >> return BoolT)
    <|> (do
            word "list"
            word "of"
            eT <- typeName True
            return $ ListT eT)
    <?> "singular type"

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

-- Parses a specific word with the possibility that it is the first word in a sentence
wordIfFirst :: String -> Bool -> FuzzyParser String
wordIfFirst (x:xs) True = word $ toUpper x : xs
wordIfFirst w False = word w

integer :: FuzzyParser Int
integer = lexeme $ L.signed (return ()) L.decimal <* notFollowedBy alphaNumChar

float :: FuzzyParser Float
float = lexeme $ L.signed (return ()) L.float <* notFollowedBy alphaNumChar

comma :: FuzzyParser ()
comma = symbol "," <?> "comma"

dot :: FuzzyParser ()
dot = symbol "." <?> "dot"

colon :: FuzzyParser ()
colon = symbol ":" <?> "colon"

parens :: FuzzyParser a -> FuzzyParser a
parens = between (symbol "(") (symbol ")")

indefiniteArticle :: Bool -> FuzzyParser String
indefiniteArticle isFirst = wordIfFirst "an" isFirst <|> wordIfFirst "a" isFirst

getCurrentLineNumber :: FuzzyParser Int
getCurrentLineNumber = unPos . sourceLine <$> getSourcePos

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
blockSentence :: FuzzyParser a -> FuzzyParser (a, LineNumber, [SentenceLine])
blockSentence pH = do
    ln <- getCurrentLineNumber
    (h, ss) <- listWithHeader pH sentence
    return (h, ln, ss)

--


-- Blocks

block :: FuzzyParser Block
block = functionDefinition

functionDefinition :: FuzzyParser Block
functionDefinition = do
    ((tl, rt), ss) <- listWithHeader functionHeader sentence
    return $ FunDef tl rt ss
    where
        functionHeader :: FuzzyParser (TitleLine, Maybe Type)
        functionHeader = do
            (Line ln fT) <- title
            (fT', rt) <- inferTypeFromTitle fT
            return (Line ln fT', rt)

-- Tries to find clues in the title of a function for its return type.
-- If no clues are found, it requires the type to be specified
-- Can modify the title after a clue is found, if applicable
inferTypeFromTitle :: Title -> FuzzyParser (Title, Maybe Type)
inferTypeFromTitle fT@(TitleWords (w:ws) : ts)
    | w == "Whether" = return (removeFirstWord fT, Just BoolT)
    | w == "To" = return (removeFirstWord fT, Nothing)
    | otherwise = (do
        comma
        word "which"
        word "results"
        word "in"
        indefiniteArticle False
        t <- typeName False
        return (fT, Just t)) <?> "return type"
    where
        removeFirstWord :: Title -> Title
        removeFirstWord (TitleWords [w] : ts) = ts
        removeFirstWord (TitleWords (w:ws) : ts) = TitleWords ws : ts

title :: FuzzyParser TitleLine
title = do
    ln <- getCurrentLineNumber
    x <- titleParam True <|> titleWords True
    xs <- case x of
        TitleWords _ -> intercalated (titleParam False) (titleWords False) <|> return []
        _ -> intercalated (titleWords False) (titleParam False)
    return $ Line ln (x:xs)

-- Parses words for an identifying part of a function's title
titleWords :: Bool -> FuzzyParser TitlePart
titleWords False = TitleWords <$> some (notFollowedBy (indefiniteArticle False) >> anyWord)
titleWords True = do
    w <- notFollowedBy (indefiniteArticle True) >> firstWord
    ws <- many (notFollowedBy (indefiniteArticle False) >> anyWord)
    return $ TitleWords (w:ws)

-- Parses a parameter of a function's title
titleParam :: Bool -> FuzzyParser TitlePart
titleParam isFirst = do
    indefiniteArticle isFirst
    t <- typeName False
    a <- parens name <?> "parameter name"
    return $ TitleParam a t

--


-- Sentences

sentence :: FuzzyParser SentenceLine
sentence =
    (variablesDefinition True <* dot)
    <|> (simpleIf True <* dot)
    <|> ifBlock
    <|> (simpleForEach True <* dot)
    <|> forEachBlock
    <|> (simpleUntil True <* dot)
    <|> untilBlock
    <|> (simpleWhile True <* dot)
    <|> whileBlock
    <|> (result True <* dot)
    <|> (sentenceMatchable <* dot)
    <?> "sentence"

-- Parses a sentence that can be used inside a simple statement
simpleSentence :: FuzzyParser SentenceLine
simpleSentence = variablesDefinition False <|> sentenceMatchable <?> "simple sentence"

-- Parses the definition of one or more variables with the same value
variablesDefinition :: Bool -> FuzzyParser SentenceLine
variablesDefinition isFirst = do
    wordIfFirst "let" isFirst
    ns <- series name
    word "be"
    ln <- getCurrentLineNumber
    Line ln . VarDef ns <$> value

simpleIf :: Bool -> FuzzyParser SentenceLine
simpleIf isFirst = do
    c <- try $ wordIfFirst "if" isFirst >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    Line ln <$> ((IfElse c [s] <$> simpleElse) <|> return (If c [s]))
    where
        simpleElse :: FuzzyParser [SentenceLine]
        simpleElse = do
            comma
            word "otherwise"
            s <- simpleSentence
            return [s]

ifBlock :: FuzzyParser SentenceLine
ifBlock = do
    (c, ln, ss) <- blockSentence (word "If" >> condition)
    Line ln <$> ((IfElse c ss <$> elseBlock) <|> return (If c ss))
    where
        elseBlock :: FuzzyParser [SentenceLine]
        elseBlock = snd <$> listWithHeader (word "otherwise") sentence

simpleForEach :: Bool -> FuzzyParser SentenceLine
simpleForEach isFirst = do
    (n, l) <- try $ forEachHeader isFirst <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (ForEach n l [s])

forEachBlock :: FuzzyParser SentenceLine
forEachBlock = do
    ((n, l), ln, ss) <- blockSentence (forEachHeader False)
    return $ Line ln (ForEach n l ss)

forEachHeader :: Bool -> FuzzyParser (Name, Value)
forEachHeader isFirst = do
    wordIfFirst "for" isFirst
    word "each"
    n <- name
    word "in"
    l <- value
    return (n, l)

simpleUntil :: Bool -> FuzzyParser SentenceLine
simpleUntil isFirst = do
    c <- try $ wordIfFirst "until" isFirst >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (Until c [s])

untilBlock :: FuzzyParser SentenceLine
untilBlock = do
    (c, ln, ss) <- blockSentence (word "Until" >> condition)
    return $ Line ln (Until c ss)

simpleWhile :: Bool -> FuzzyParser SentenceLine
simpleWhile isFirst = do
    c <- try $ wordIfFirst "while" isFirst >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (While c [s])

whileBlock :: FuzzyParser SentenceLine
whileBlock = do
    (c, ln, ss) <- blockSentence (word "While" >> condition)
    return $ Line ln (While c ss)

-- Parses a return statement
result :: Bool -> FuzzyParser SentenceLine
result isFirst = do
    try $ wordIfFirst "the" isFirst >> word "result"
    ln <- getCurrentLineNumber
    word "is"
    Line ln . Result <$> value

sentenceMatchable :: FuzzyParser SentenceLine
sentenceMatchable = do
    ln <- getCurrentLineNumber
    p <- matchablePart True
    ps <- many $ matchablePart False
    return $ Line ln (SentenceM $ p:ps)

--


-- Values

value :: FuzzyParser Value
value = listValue <|> valueMatchable

-- Parses a list with matchables as elements
listValue :: FuzzyParser Value
listValue = do
    try $ word "a" >> word "list"
    word "of"
    t <- typeName True
    l <- (word "containing" >> series valueMatchable) <|> return []
    return $ ListV t l

valueMatchable :: FuzzyParser Value
valueMatchable = ValueM <$> some (matchablePart False)

condition :: FuzzyParser Value
condition = valueMatchable

matchablePart :: Bool -> FuzzyParser MatchablePart
matchablePart isFirst =
    try (FloatP <$> float)
    <|> IntP <$> integer
    <|> WordP <$> (if isFirst then firstWord else anyWord)
    <|> ParensP <$> parens (some $ matchablePart False)
    <?> "valid term"

--


-- Main

-- Parses a string using a specific parser and returns its result or the error it yielded
runFuzzyParser :: FuzzyParser a -> String -> Either String a
runFuzzyParser p s =
    case parse p "" s of
        Left e -> Left $ errorBundlePretty e
        Right a -> Right a

-- Returns the program parsed from a given source code
parseProgram :: String -> Program
parseProgram s =
    case FuzzyParser.runFuzzyParser parseProgram' s of
        Left e -> error e
        Right r -> r
    where
        parseProgram' :: FuzzyParser Program
        parseProgram' = some block <* eof

--
