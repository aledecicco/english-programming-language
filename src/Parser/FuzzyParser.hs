{-# LANGUAGE TupleSections #-}

module FuzzyParser where

import Data.Void (Void)
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

-- Parses a case insensitive string consuming trailing whitespace
symbol :: String -> FuzzyParser ()
symbol = void . L.symbol' sc

reservedWords :: [String]
reservedWords = ["be", "in", "is", "an", "a", "containing"]

--


-- General

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
    <?> "singular type"
typeName False =
    (word "integer" >> return IntT)
    <|> (word "float" >> return FloatT)
    <|> (word "boolean" >> return BoolT)
    <|> (do
            word "list"
            word "of"
            eT <- typeName True
            return $ ListT eT)
    <?> "plural type"

-- Parses any word and checks that it's not reserved
identifier :: FuzzyParser String
identifier = do
    w <- anyWord
    if w `elem` reservedWords
        then fail $ "Incorrect use of reserved word \"" ++ w ++ "\""
        else return w

-- Parses a specific word
word :: String -> FuzzyParser String
word w = lexeme $ string' w <* notFollowedBy alphaNumChar

integer :: FuzzyParser Integer
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

indefiniteArticle :: FuzzyParser String
indefiniteArticle = word "an" <|> word "a"

getCurrentLineNumber :: FuzzyParser Int
getCurrentLineNumber = unPos . sourceLine <$> getSourcePos

--


-- Combinators

-- Parses a series of a given parser (such as "a, b and c")
series :: FuzzyParser a -> FuzzyParser [a]
series p = do
    x <- p;
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
intercalated pA pB = (:) <$> pA <*> (try (intercalated pB pA) <|> return [])

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
    | w == "whether" = return (removeFirstWord fT, Just BoolT)
    | w == "to" = return (removeFirstWord fT, Nothing)
    | otherwise = do
        comma
        word "which"
        word "results"
        word "in"
        indefiniteArticle
        t <- typeName False
        return (fT, Just t)
    where
        removeFirstWord :: Title -> Title
        removeFirstWord (TitleWords [w] : ts) = ts
        removeFirstWord (TitleWords (w:ws) : ts) = TitleWords ws : ts

title :: FuzzyParser TitleLine
title = do
    ln <- getCurrentLineNumber
    x <- titleParam <|> titleWords
    xs <- case x of
        TitleWords _ -> intercalated titleParam titleWords <|> return []
        _ -> intercalated titleWords titleParam
    return $ Line ln (x:xs)

-- Parses words for an identifying part of a function's title
titleWords :: FuzzyParser TitlePart
titleWords = TitleWords <$> some (notFollowedBy indefiniteArticle >> anyWord)

-- Parses a parameter of a function's title
titleParam :: FuzzyParser TitlePart
titleParam = do
    indefiniteArticle
    t <- typeName False
    a <- parens name
    return $ TitleParam a t

--


-- Sentences

sentence :: FuzzyParser SentenceLine
sentence =
    (variablesDefinition <* dot)
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
simpleSentence :: FuzzyParser SentenceLine
simpleSentence = variablesDefinition <|> sentenceMatchable <?> "simple sentence"

-- Parses the definition of one or more variables with the same value
variablesDefinition :: FuzzyParser SentenceLine
variablesDefinition = do
    word "let"
    ns <- series name
    word "be"
    ln <- getCurrentLineNumber
    Line ln . VarDef ns <$> value

simpleIf :: FuzzyParser SentenceLine
simpleIf = do
    c <- try $ word "if" >> condition <* comma
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
    (c, ln, ss) <- blockSentence (word "if" >> condition)
    Line ln <$> ((IfElse c ss <$> elseBlock) <|> return (If c ss))
    where
        elseBlock :: FuzzyParser [SentenceLine]
        elseBlock = snd <$> listWithHeader (word "otherwise") sentence

simpleForEach :: FuzzyParser SentenceLine
simpleForEach = do
    (n, l) <- try $ forEachHeader <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (ForEach n l [s])

forEachBlock :: FuzzyParser SentenceLine
forEachBlock = do
    ((n, l), ln, ss) <- blockSentence forEachHeader
    return $ Line ln (ForEach n l ss)

forEachHeader :: FuzzyParser (Name, Value)
forEachHeader = do
    word "for"
    word "each"
    n <- name
    word "in"
    l <- value
    return (n, l)

simpleUntil :: FuzzyParser SentenceLine
simpleUntil = do
    c <- try $ word "until" >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (Until c [s])

untilBlock :: FuzzyParser SentenceLine
untilBlock = do
    (c, ln, ss) <- blockSentence (word "until" >> condition)
    return $ Line ln (Until c ss)

simpleWhile :: FuzzyParser SentenceLine
simpleWhile = do
    c <- try $ word "while" >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (While c [s])

whileBlock :: FuzzyParser SentenceLine
whileBlock = do
    (c, ln, ss) <- blockSentence (word "while" >> condition)
    return $ Line ln (While c ss)

-- Parses a return statement
result :: FuzzyParser SentenceLine
result = do
    try (word "the" >> word "result")
    ln <- getCurrentLineNumber
    word "is"
    Line ln . Result <$> value

sentenceMatchable :: FuzzyParser SentenceLine
sentenceMatchable = do
    ln <- getCurrentLineNumber
    Line ln . SentenceM <$> some matchablePart

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
valueMatchable = ValueM <$> some matchablePart

condition :: FuzzyParser Value
condition = valueMatchable

matchablePart :: FuzzyParser MatchablePart
matchablePart =
    try (FloatP <$> float)
    <|> IntP <$> integer
    <|> WordP <$> anyWord
    <|> ParensP <$> parens (some matchablePart)
    <?> "valid term"

--


-- Main

-- Parses a string using a specific parser and returns its result or the error it yielded
runParser :: FuzzyParser a -> String -> Either String a
runParser p s =
    case parse p "" s of
        Left e -> Left $ errorBundlePretty e
        Right a -> Right a

-- Returns the program parsed from a given source code
parseProgram :: String -> Program
parseProgram s =
    case FuzzyParser.runParser parseProgram' s of
        Left e -> error e
        Right r -> r
    where
        parseProgram' :: FuzzyParser Program
        parseProgram' = some block <* eof

--
