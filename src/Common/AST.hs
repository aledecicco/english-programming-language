{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE LiberalTypeSynonyms  #-}
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

-- | Parts that together can be interpreted as different expressions.
data MatchablePart a = IntP a Int | FloatP a Float | CharP a Char | StringP a String | WordP a String | ParensP [MatchablePart a]
    deriving (Eq, Show, Functor, Foldable)

type LineNumber = Int
type ColumnNumber = Int

-- | A source code location.
type Location = (LineNumber, ColumnNumber)

-- | Transforms a parametrized type into a type that contains locations.
-- For example, @Annotated Value@ represents a value with annotations for it and all its children.
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
    | AnyT String -- ^ A type variable identified by a string.
    deriving (Eq, Show)


-- -----------------
-- * Program structure

data Value a =
    ValueM a [MatchablePart a] -- ^ A list of matchable parts that are yet to be converted into a concrete value.
    | IntV a Int
    | FloatV a Float
    | BoolV a Bool
    | CharV a Char
    | ListV a Type [Value a] -- ^ A list containing values of a given type.
    | VarV a Name
    | RefV a Int -- ^ A reference to a memory address.
    | IterV a Type (Value a) -- ^ Makes the structure it is contained in to be applied to all the elements in the list value it contains.
    | OperatorCall a FunId [Value a]
    deriving (Eq, Show, Functor, Foldable)

-- | Expressions in the language.
data Sentence a =
    SentenceM a [MatchablePart a] -- ^ A list of matchable parts that are yet to be converted into a concrete sentence.
    | VarDef a [Name] (Maybe Type) (Value a) -- ^ The definition of a new variable.
    | When a (Value a) [Sentence a]
    | Unless a (Value a) [Sentence a]
    | IfElse a (Value a) [Sentence a] [Sentence a]
    | ForEach a [Name] Type (Value a) [Sentence a]
    | While a (Value a) [Sentence a]
    | Until a (Value a) [Sentence a]
    | Return a (Value a)
    | ProcedureCall a FunId [Value a]
    | Read a Type (Value a)
    | Attempt a [Sentence a]
    | TryCatch a [Sentence a] [Sentence a]
    | Throw a [String]
    | Break a
    | Exit a
    deriving (Eq, Show, Functor, Foldable)

data TitlePart a =
    TitleWords a [String] -- ^ Words that act as the identifier of a function.
    | TitleParam a [Name] Type -- ^ A parameter that a function can receive.
    deriving (Eq, Show, Functor, Foldable)

-- | The signature of a function minus the return type.
type Title a = [TitlePart a]

data FunSignature = FunSignature (Bare Title) FunType

-- | The neccessary information to call a function.
data FunCallable = FunCallable (Bare Title) [Annotated Sentence]

data FunType =
    Operator ([Type] -> Type) -- ^ Each operator has a function that transforms the types of its arguments into the type of its result.
    | Procedure

data Definition a =
    FunDef a (Title a) (Maybe Type) [Sentence a] -- ^ An user defined function that can only have a basic return type.
                                                 -- If the type is Nothing, it represents a procedure.
    deriving (Eq, Show, Functor, Foldable)

type Program = [Annotated Definition]
