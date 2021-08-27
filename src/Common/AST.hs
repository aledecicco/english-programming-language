{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module AST where

--


-- General

type Name = [String]

type FunId = String

data MatchablePart a = IntP a Int | FloatP a Float | CharP a Char | StringP a String | WordP a String | ParensP [MatchablePart a]
    deriving (Eq, Show, Functor, Foldable)

--


-- Program structure

-- (Line number, Column number)
type Location = (Int, Int)
type Annotated a = a Location
type Bare a = a ()

data Type = IntT | FloatT | BoolT | CharT | ListT Type | RefT Type | AnyT String
    deriving (Eq, Show)

data Value a =
    ValueM a [MatchablePart a]
    | IntV a Int | FloatV a Float | BoolV a Bool | CharV a Char
    | ListV a Type [Value a] | VarV a Name | RefV a Int | IterV a Type (Value a)
    | OperatorCall a FunId [Value a]
    deriving (Eq, Show, Functor, Foldable)

data Sentence a =
    SentenceM a [MatchablePart a]
    | VarDef a [Name] (Maybe Type) (Value a)
    | If a (Value a) [Sentence a] | IfElse a (Value a) [Sentence a] [Sentence a]
    | ForEach a Name Type (Value a) [Sentence a] | Until a (Value a) [Sentence a] | While a (Value a) [Sentence a]
    | Return a (Value a)
    | ProcedureCall a FunId [Value a]
    | Try a [Sentence a] | TryCatch a [Sentence a] [Sentence a] | Throw a [String]
    deriving (Eq, Show, Functor, Foldable)

data TitlePart a = TitleWords a [String] | TitleParam a [Name] Type
    deriving (Eq, Show, Functor, Foldable)

data Title a = Title a [TitlePart a]
    deriving (Eq, Show, Functor, Foldable)

data FunSignature = FunSignature (Bare Title) FunType

data FunCallable = FunCallable (Bare Title) [Annotated Sentence]

data FunType = Operator ([Type] -> Type) | Procedure

data Block a = FunDef a (Title a) (Maybe Type) [Sentence a]
    deriving (Eq, Show, Functor, Foldable)

type Program = [Annotated Block]

--
