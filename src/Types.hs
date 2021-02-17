module Types where

type Name = [String]

type LineNumber = Int

data Type = IntT | FloatT | BoolT | ListT Type | AnyT
    deriving (Eq, Show)

data MatchablePart = IntP Integer | FloatP Float | WordP String | ParensP [MatchablePart]
    deriving (Eq, Show)

data Value =
    ValueM [MatchablePart]
    | IntV Integer | FloatV Float | BoolV Bool
    | ListV Type [Value] | VarV Name
    | OperatorCall Title [Value]
    deriving (Eq, Show)

data Sentence =
    SentenceM [MatchablePart]
    | VarDef [Name] Value
    | If Value [SentenceLine] | IfElse Value [SentenceLine] [SentenceLine]
    | ForEach Name Value [SentenceLine] | Until Value [SentenceLine] | While Value [SentenceLine]
    | Result Value
    | ProcedureCall Title [Value]
    deriving (Eq, Show)

data Line a = Line LineNumber a deriving (Eq, Show)

type SentenceLine = Line Sentence

type TitleLine = Line Title

getLineNumber :: Line a -> LineNumber
getLineNumber (Line ln _) = ln

getLineContent :: Line a -> a
getLineContent (Line _ c) = c

data TitlePart = TitleWords [String] | TitleParam Name Type
    deriving (Eq, Show)

type Title = [TitlePart]

data Function = Operator Title ([Type] -> Type) | Procedure Title

data Block = FunDef TitleLine [SentenceLine]
    deriving (Eq, Show)

type Program = [Block]
