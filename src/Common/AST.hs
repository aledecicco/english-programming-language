{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-|
Module      : AST
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Abstract Syntax Tree of the language and other useful types.
-}

module AST where


-- -----------------
-- * General types

type Name = [String]

type FunId = String

-- | Parts that together can be interpreted as different expressions
data MatchablePart a = IntP a Int | FloatP a Float | CharP a Char | StringP a String | WordP a String | ParensP [MatchablePart a]
    deriving (Eq, Show, Functor, Foldable)


-- -----------------
-- * Program structure

type LineNumber = Int
type ColumnNumber = Int

-- | A source code location.
type Location = (LineNumber, ColumnNumber)

-- | Transforms a parametrized type into a type that contains locations.
type Annotated a = a Location

-- | Transforms a parametrized type into a type that contains nothing.
type Bare a = a ()

-- | The types that a value can have.
data Type =
    IntT
    | FloatT
    | BoolT
    | CharT
    | ListT Type
    | RefT Type
    -- | A type variable identified by a string.
    | AnyT String
    deriving (Eq, Show)

data Value a =
    -- | A list of matchable parts that are yet to be converted into a concrete value.
    ValueM a [MatchablePart a]
    | IntV a Int
    | FloatV a Float
    | BoolV a Bool
    | CharV a Char
    -- | A list containing values of a given type.
    | ListV a Type [Value a]
    | VarV a Name
    -- | A reference to a memory address.
    | RefV a Int
    -- | Makes the structure it is contained in to be applied to all the elements in the list value it contains.
    | IterV a Type (Value a)
    | OperatorCall a FunId [Value a]
    deriving (Eq, Show, Functor, Foldable)

-- | Expressions in the language.
data Sentence a =
    -- | A list of matchable parts that are yet to be converted into a concrete sentence.
    SentenceM a [MatchablePart a]
    -- | The definition of a new variable.
    | VarDef a [Name] (Maybe Type) (Value a)
    | If a (Value a) [Sentence a]
    | IfElse a (Value a) [Sentence a] [Sentence a]
    | ForEach a Name Type (Value a) [Sentence a]
    | Until a (Value a) [Sentence a]
    | While a (Value a) [Sentence a]
    | Return a (Value a)
    | ProcedureCall a FunId [Value a]
    | Try a [Sentence a]
    | TryCatch a [Sentence a] [Sentence a]
    | Throw a [String]
    deriving (Eq, Show, Functor, Foldable)

data TitlePart a =
    -- | Words that act as the identifier of a function.
    TitleWords a [String]
    -- | A parameter that a function can receive.
    | TitleParam a [Name] Type
    deriving (Eq, Show, Functor, Foldable)

-- | The signature of a function minus the return type.
data Title a = Title a [TitlePart a]
    deriving (Eq, Show, Functor, Foldable)

data FunSignature = FunSignature (Bare Title) FunType

-- | The neccessary information to call a function.
data FunCallable = FunCallable (Bare Title) [Annotated Sentence]

data FunType =
    -- | Each operator has a function that transforms the types of its arguments into the type of its result.
    Operator ([Type] -> Type)
    -- | Procedures can't return anything.
    | Procedure

-- | All possible blocks in a program.
data Block a =
    -- | An user defined function that can only have a basic return type.
    -- If the type is Nothing, it represents a procedure.
    FunDef a (Title a) (Maybe Type) [Sentence a]
    deriving (Eq, Show, Functor, Foldable)

type Program = [Annotated Block]
