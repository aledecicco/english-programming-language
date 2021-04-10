module AST where

--


-- General

type Name = [String]

type FunId = String

data MatchablePart = IntP Int | FloatP Float | CharP Char | StringP String | WordP String | ParensP [MatchablePart]
    deriving (Eq, Show)

--


-- Program structure

data Type = IntT | FloatT | BoolT | CharT | ListT Type | AnyT String
    deriving (Eq, Show)

data Value =
    ValueM [MatchablePart]
    | IntV Int | FloatV Float | BoolV Bool | CharV Char
    | ListV Type [Value] | VarV Name
    | OperatorCall FunId [Value]
    deriving (Eq, Show)

data Sentence =
    SentenceM [MatchablePart]
    | VarDef [Name] Value
    | If Value [SentenceLine] | IfElse Value [SentenceLine] [SentenceLine]
    | ForEach Name Value [SentenceLine] | Until Value [SentenceLine] | While Value [SentenceLine]
    | Result Value
    | ProcedureCall FunId [Value]
    deriving (Eq, Show)

data TitlePart = TitleWords [String] | TitleParam Name Type
    deriving (Eq, Show)

type Title = [TitlePart]

data FunSignature = FunSignature Title FunType

data FunCallable = FunCallable Title [SentenceLine]

data FunType = Operator ([Type] -> Type) | Procedure


data Block = FunDef TitleLine (Maybe Type) [SentenceLine]
    deriving (Eq, Show)

type Program = [Block]

--


-- Lines

type LineNumber = Int

data Line a = Line LineNumber a deriving (Eq, Show)

type SentenceLine = Line Sentence

type TitleLine = Line Title

--
