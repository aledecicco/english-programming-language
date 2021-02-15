module Types where

type Name = [String]

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
    | If Value [Sentence] | IfElse Value [Sentence] [Sentence]
    | For Name Value Value [Sentence] | ForEach Name Value [Sentence]
    | Until Value [Sentence] | While Value [Sentence]
    | Result Value
    | ProcedureCall Title [Value]
    deriving (Eq, Show)

data Block = FunDef Title [Sentence]
    deriving (Eq, Show)

type Program = [Block]

data TitlePart = TitleWords [String] | TitleParam Name Type
    deriving (Eq, Show)

type Title = [TitlePart]

data Function = Operator Title ([Type] -> Type) | Procedure Title
