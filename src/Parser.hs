{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Void (Void)
import Control.Monad (void, when)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types
import Utils ( isWord )

--


-- Parser definitions

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

-- Consumes whitespace including new lines
scn :: Parser ()
scn = L.space space1 empty empty

-- Consumes whitespace not including new lines
sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty

-- Wrapper for parsing lexemes consumig trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parses a case insensitive string consuming trailing whitespace
symbol :: String -> Parser ()
symbol = void . L.symbol' sc

--


-- General

reservedWords :: [String]
reservedWords = ["be", "in", "is", "an", "a", "containing"]

anyWord :: Parser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <?> "word"

-- Parses a name (list of words that are not reserved)
name :: Parser Name
name = (some . try) identifier <?> "name"

-- Parses a type by its name
typeName :: Bool -> Parser Type
typeName True =
    (reserved "integers" >> return IntT)
    <|> (reserved "floats" >> return FloatT)
    <|> (reserved "booleans" >> return BoolT)
    <|> (do
            reserved "lists"
            reserved "of"
            eT <- typeName True
            return $ ListT eT)
    <?> "singular type"
typeName False =
    (reserved "integer" >> return IntT)
    <|> (reserved "float" >> return FloatT)
    <|> (reserved "boolean" >> return BoolT)
    <|> (do
            reserved "list"
            reserved "of"
            eT <- typeName True
            return $ ListT eT)
    <?> "plural type"


-- Parses a word and checks that it's not reserved
identifier :: Parser String
identifier = do
    w <- anyWord
    if any (\w' -> w `isWord` w') reservedWords
    then fail $ "Incorrect use of reserved word \"" ++ w ++ "\""
    else return w

-- Parses a specific word
reserved :: String -> Parser String
reserved w = lexeme $ string' w <* notFollowedBy alphaNumChar

integer :: Parser Integer
integer = lexeme $ L.signed (return ()) L.decimal <* notFollowedBy alphaNumChar

float :: Parser Float
float = lexeme $ L.signed (return ()) L.float <* notFollowedBy alphaNumChar

comma :: Parser ()
comma = symbol "," <?> "comma"

dot :: Parser ()
dot = symbol "." <?> "dot"

colon :: Parser ()
colon = symbol ":" <?> "colon"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

indefiniteArticle :: Parser String
indefiniteArticle = reserved "an" <|> reserved "a"

-- Parses a series of a given parser (such as "a, b and c")
series :: Parser a -> Parser [a]
series p = do
    x <- p;
    xs <- series' p <|> return []
    return $ x:xs
    where
        series' :: Parser a -> Parser [a]
        series' p = do
            comma
            xs <- (many . try) $ p <* comma
            reserved "and"
            x' <- p
            return $ xs++[x']

-- Parses an intercalated list of two parsers
intercalated :: Parser a -> Parser a -> Parser [a]
intercalated pA pB = (:) <$> pA <*> (try (intercalated pB pA) <|> return [])

-- Parses a header followed by a block of indented elements
listWithHeader :: Parser a -> Parser b -> Parser (a, [b])
listWithHeader pH pE = L.indentBlock scn listWithHeader'
    where
        listWithHeader' = do
            h <- pH
            colon
            return $ L.IndentSome Nothing (return . (h, )) pE

-- Parses a specific sentence in block format
blockSentence :: Parser a -> Parser (a, LineNumber, [SentenceLine])
blockSentence pH = do
    ln <- getCurrentLineNumber
    (h, ss) <- listWithHeader pH sentence
    return (h, ln, ss)

getCurrentLineNumber :: Parser Int
getCurrentLineNumber = unPos . sourceLine <$> getSourcePos

--


-- Blocks

block :: Parser Block
block = functionDefinition

functionDefinition :: Parser Block
functionDefinition = do
    ((tl, rt), ss) <- listWithHeader functionHeader sentence
    return $ FunDef tl rt ss
    where
        functionHeader :: Parser (TitleLine, Maybe Type)
        functionHeader = do
            (Line ln fT) <- title
            (fT', rt) <- inferTypeFromTitle fT
            return (Line ln fT', rt)

-- Tries to find clues in the title of a function for its return type.
-- If no clues are found, it requires the type to be specified
-- Can modify the title after a clue is found, if applicable
inferTypeFromTitle :: Title -> Parser (Title, Maybe Type)
inferTypeFromTitle fT@(TitleWords (w:ws) : ts)
    | w `isWord` "whether" = return (removeFirstWord fT, Just BoolT)
    | w `isWord` "to" = return (removeFirstWord fT, Nothing)
    | otherwise = do
        comma
        reserved "which"
        reserved "results"
        reserved "in"
        indefiniteArticle
        t <- typeName False
        return (fT, Just t)
    where
        removeFirstWord :: Title -> Title
        removeFirstWord (TitleWords [w] : ts) = ts
        removeFirstWord (TitleWords (w:ws) : ts) = TitleWords ws : ts

title :: Parser TitleLine
title = do
    ln <- getCurrentLineNumber
    x <- titleParam <|> titleWords
    xs <- case x of
        TitleWords _ -> intercalated titleParam titleWords <|> return []
        _ -> intercalated titleWords titleParam
    return $ Line ln (x:xs)

-- Parses words for an identifying part of a function's title
titleWords :: Parser TitlePart
titleWords = TitleWords <$> some (notFollowedBy indefiniteArticle >> anyWord)

-- Parses a parameter of a function's title
titleParam :: Parser TitlePart
titleParam = do
    indefiniteArticle
    t <- typeName False
    a <- parens name
    return $ TitleParam a t

--


-- Sentences

sentence :: Parser SentenceLine
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
simpleSentence :: Parser SentenceLine
simpleSentence = variablesDefinition <|> sentenceMatchable <?> "simple sentence"

-- Parses the definition of one or more variables with the same value
variablesDefinition :: Parser SentenceLine
variablesDefinition = do
    reserved "let"
    ns <- series name
    reserved "be"
    ln <- getCurrentLineNumber
    Line ln . VarDef ns <$> value

simpleIf :: Parser SentenceLine
simpleIf = do
    c <- try $ reserved "if" >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    Line ln <$> ((IfElse c [s] <$> simpleElse) <|> return (If c [s]))
    where
        simpleElse :: Parser [SentenceLine]
        simpleElse = do
            comma
            reserved "otherwise"
            s <- simpleSentence
            return [s]

ifBlock :: Parser SentenceLine
ifBlock = do
    (c, ln, ss) <- blockSentence (reserved "if" >> condition)
    Line ln <$> ((IfElse c ss <$> elseBlock) <|> return (If c ss))
    where
        elseBlock :: Parser [SentenceLine]
        elseBlock = snd <$> listWithHeader (reserved "otherwise") sentence

simpleForEach :: Parser SentenceLine
simpleForEach = do
    (n, l) <- try $ forEachHeader <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (ForEach n l [s])

forEachBlock :: Parser SentenceLine
forEachBlock = do
    ((n, l), ln, ss) <- blockSentence forEachHeader
    return $ Line ln (ForEach n l ss)

forEachHeader :: Parser (Name, Value)
forEachHeader = do
    reserved "for"
    reserved "each"
    n <- name
    reserved "in"
    l <- value
    return (n, l)

simpleUntil :: Parser SentenceLine
simpleUntil = do
    c <- try $ reserved "until" >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (Until c [s])

untilBlock :: Parser SentenceLine
untilBlock = do
    (c, ln, ss) <- blockSentence (reserved "until" >> condition)
    return $ Line ln (Until c ss)

simpleWhile :: Parser SentenceLine
simpleWhile = do
    c <- try $ reserved "while" >> condition <* comma
    ln <- getCurrentLineNumber
    s <- simpleSentence
    return $ Line ln (While c [s])

whileBlock :: Parser SentenceLine
whileBlock = do
    (c, ln, ss) <- blockSentence (reserved "while" >> condition)
    return $ Line ln (While c ss)

-- Parses a return statement
result :: Parser SentenceLine
result = do
    try (reserved "the" >> reserved "result")
    ln <- getCurrentLineNumber
    reserved "is"
    Line ln . Result <$> value

sentenceMatchable :: Parser SentenceLine
sentenceMatchable = do
    ln <- getCurrentLineNumber
    Line ln . SentenceM <$> some matchablePart

--


-- Values

value :: Parser Value
value = listValue <|> valueMatchable

-- Parses a list with matchables as elements
listValue :: Parser Value
listValue = do
    try $ reserved "a" >> reserved "list"
    reserved "of"
    t <- typeName True
    l <- (reserved "containing" >> series valueMatchable) <|> return []
    return $ ListV t l

valueMatchable :: Parser Value
valueMatchable = ValueM <$> some matchablePart

condition :: Parser Value
condition = valueMatchable

matchablePart :: Parser MatchablePart
matchablePart =
    try (FloatP <$> float)
    <|> IntP <$> integer
    <|> WordP <$> anyWord
    <|> ParensP <$> parens (some matchablePart)
    <?> "valid term"

--


-- Main

-- Parses a string using a specific parser and returns its result or the error it yielded
parse :: Parser a -> String -> Either String a
parse p s =
    case Text.Megaparsec.parse p "" s of
        Left e -> Left $ errorBundlePretty e
        Right b -> Right b

-- Returns the program parsed from a given source code
parseProgram :: String -> Program
parseProgram s =
    case Parser.parse (some block <* eof) s of
        Left e -> error e
        Right b -> b

--
