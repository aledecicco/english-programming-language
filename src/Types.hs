module Types where

type Name = [String]

data Type = IntT | BoolT | StringT | StructT Name | ListT Type | TypeM Name | AnyT String
    deriving (Eq, Show)

data MatchablePart = IntP Integer | LiteralP String | WordP String | PossessiveP | ParensP [MatchablePart]
    deriving (Eq, Show)

data Value =
    ValueM [MatchablePart]
    | ListM [Value]
    | IntV Integer | BoolV Bool | StringV String
    | StructV Name [(Name, Value)] | ListV Type [Value]
    | VarV Name | PossessiveV Value Name
    | OperatorCall Title [Value]
    deriving (Eq, Show)

data Sentence =
    SentenceM [MatchablePart]
    | VarDef [Name] Value
    | If Value [Sentence] | IfElse Value [Sentence] [Sentence]
    | For Name Value Value [Sentence] | ForEach Name Value [Sentence]
    | Until Value [Sentence] | While Value [Sentence]
    | Result Type Value
    | ProcedureCall Title [Value]
    deriving (Eq, Show)

data Block = FunDef Title [Sentence] | StructDef Name Name [(Name, Type)]
    deriving (Eq, Show)

type Program = [Block]

data TitlePart = UnnamedTitleParamM Name | NamedTitleParamM Name Type | TitleWords [String] | TitleParam Name Type
    deriving (Eq, Show)

type Title = [TitlePart]

data Function = Operator Title ([Type] -> Type) | Procedure Title
