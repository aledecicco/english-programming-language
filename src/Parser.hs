{-# LANGUAGE TupleSections #-}

module Parser where

import Data.Void (Void)
import Control.Monad (void, when)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

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
reservedWords = ["be", "and", "if", "from", "to", "in", "is", "an", "a", "containing", "with", "equal"]

anyWord :: Parser String
anyWord = lexeme (some letterChar <* notFollowedBy alphaNumChar) <?> "word"

-- Parses a name (list of words that are not reserved)
name :: Parser Name
name = (some . try) identifier <?> "name"

-- Parses a type by its name
typeName :: Bool -> Parser Type
typeName True =
    (reserved "integers" >> return IntT)
    <|> (reserved "booleans" >> return BoolT)
    <|> (reserved "strings" >> return StringT)
    <|> (do
            reserved "lists"
            reserved "of"
            eT <- typeName True
            return $ ListT eT)
typeName False =
    (reserved "integer" >> return IntT)
    <|> (reserved "boolean" >> return BoolT)
    <|> (reserved "string" >> return StringT)
    <|> (do
            reserved "list"
            reserved "of"
            eT <- typeName True
            return $ ListT eT)


-- Parses a word and checks that it's not reserved
identifier :: Parser String
identifier = do
    w <- anyWord
    if w `elem` reservedWords
    then fail $ "Incorrect use of reserved word \"" ++ w ++ "\""
    else return w

-- Parses a specific word
reserved :: String -> Parser ()
reserved w = lexeme $ string' w >> notFollowedBy alphaNumChar

integer :: Parser Integer
integer = lexeme $ L.signed empty L.decimal <* notFollowedBy alphaNumChar

comma :: Parser ()
comma = symbol "," <?> "comma"

dot :: Parser ()
dot = symbol "." <?> "dot"

colon :: Parser ()
colon = symbol ":" <?> "colon"

stringLiteral :: Parser String
stringLiteral = lexeme $ char '\"' >> manyTill L.charLiteral (char '\"')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

indefiniteArticle :: Parser ()
indefiniteArticle = reserved "an" <|> reserved "a"

-- Parses a series of a given parser (such as "a, b and c")
series :: Parser a -> Parser [a]
series p = do
    x <- p;
    xs <- series' <|> return []
    return $ x:xs
    where
        series' = do
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

--


-- Blocks

block :: Parser Block
block = functionDefinition

functionDefinition :: Parser Block
functionDefinition = do
    (t, ss) <- listWithHeader title sentence
    return $ FunDef t ss

title :: Parser Title
title = do
    x <- titleParam <|> titleWords
    case x of
        TitleWords _ -> (x:) <$> (intercalated titleParam titleWords <|> return [])
        _ -> (x:) <$> intercalated titleWords titleParam

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

sentence :: Parser Sentence
sentence =
    (variablesDefinition <* dot)
    <|> (simpleIf <* dot)
    <|> ifBlock
    <|> (simpleForEach <* dot)
    <|> forEachBlock
    <|> (simpleFor <* dot)
    <|> forEachBlock
    <|> (simpleUntil <* dot)
    <|> untilBlock
    <|> (simpleWhile <* dot)
    <|> whileBlock
    <|> (result <* dot)
    <|> (sentenceMatchable <* dot)
    <?> "sentence"

-- Parses a sentence that can be used inside a simple statement
simpleSentence :: Parser Sentence
simpleSentence = variablesDefinition <|> sentenceMatchable <?> "simple sentence"

-- Parses the definition of one or more variables with the same value
variablesDefinition :: Parser Sentence
variablesDefinition = do
    reserved "let"
    ns <- series name
    reserved "be"
    VarDef ns <$> value

simpleIf :: Parser Sentence
simpleIf = do
    c <- try $ reserved "if" >> condition <* comma
    s <- simpleSentence
    (IfElse c [s] <$> simpleElse) <|> return (If c [s])
    where
        simpleElse :: Parser [Sentence]
        simpleElse = do
            comma
            reserved "otherwise"
            s <- simpleSentence
            return [s]

ifBlock :: Parser Sentence
ifBlock = do
    (c, ss) <- listWithHeader (reserved "if" >> condition) sentence
    (IfElse c ss <$> elseBlock) <|> return (If c ss)
    where
        elseBlock :: Parser [Sentence]
        elseBlock = snd <$> listWithHeader (reserved "otherwise") sentence

simpleForEach :: Parser Sentence
simpleForEach = do
    (n, l) <- try $ forEachHeader <* comma
    s <- simpleSentence
    return $ ForEach n l [s]

forEachBlock :: Parser Sentence
forEachBlock = do
    ((n, l), ss) <- listWithHeader forEachHeader sentence
    return $ ForEach n l ss

forEachHeader :: Parser (Name, Value)
forEachHeader = do
    reserved "for"
    reserved "each"
    n <- name
    reserved "in"
    l <- value
    return (n, l)

simpleFor :: Parser Sentence
simpleFor = do
    (i, vf, vt) <- try $ forHeader <* comma
    s <- simpleSentence
    return $ For i vf vt [s]

forBlock :: Parser Sentence
forBlock = do
    ((i, vf, vt), ss) <- listWithHeader forHeader sentence
    return $ For i vf vt ss

forHeader :: Parser (Name, Value, Value)
forHeader = do
    reserved "for"
    i <- (try indefiniteArticle >> name) <|> name
    reserved "from"
    vf <- valueMatchable
    reserved "to"
    vt <- valueMatchable
    return (i, vf, vt)

simpleUntil :: Parser Sentence
simpleUntil = do
    c <- try $ reserved "until" >> condition <* comma
    s <- simpleSentence
    return $ Until c [s]

untilBlock :: Parser Sentence
untilBlock = do
    (c, ss) <- listWithHeader (reserved "until" >> condition) sentence
    return $ Until c ss

simpleWhile :: Parser Sentence
simpleWhile = do
    c <- try $ reserved "while" >> condition <* comma
    s <- simpleSentence
    return $ While c [s]

whileBlock :: Parser Sentence
whileBlock = do
    (c, ss) <- listWithHeader (reserved "while" >> condition) sentence
    return $ While c ss

-- Parses a return statement
result :: Parser Sentence
result = do
    reserved "the"
    reserved "result"
    reserved "is"
    Result <$> value

sentenceMatchable :: Parser Sentence
sentenceMatchable = SentenceM <$> some matchablePart

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
    IntP <$> integer
    <|> LiteralP <$> stringLiteral
    <|> WordP <$> anyWord
    <|> ParensP <$> parens (some matchablePart)
    <?> "valid term"

--


-- Main

-- Returns the program parsed from a given source code
parseProgram :: String -> Program
parseProgram s = case parse (some block <* eof) "" s of
    Left e -> error $ errorBundlePretty e
    Right b -> b

--
