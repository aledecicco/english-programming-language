{-|
Module      : Errors
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Errors, warnings, and their types.
-}

module Errors where

import AST


data ErrorType =
    -- | Expected a value to have a different type.
    WrongTypeValue Type Type
    -- | Expected a function parameter to have a different type.
    | WrongTypeParameter Type Type Int FunId
    -- | Couldn't understand a list of matchable parts as any value.
    | UnmatchableValue [Annotated MatchablePart]
    -- | A list of matchable parts could be understood as many different values but none of them passed the validations.
    | UnmatchableValueTypes [Annotated MatchablePart]
    -- | Couldn't understand a list of matchable parts as any sentence.
    | UnmatchableSentence [Annotated MatchablePart]
    -- | A list of matchable parts could be understood as many different sentences but none of them passed the validations.
    | UnmatchableSentenceTypes [Annotated MatchablePart]
    | FunctionAlreadyDefined FunId
    | UndefinedFunction FunId
    | VariableAlreadyDefined Name
    | UndefinedVariable Name
    -- | Assigned a value to a variable of a different type.
    | MismatchingTypeAssigned Type Type Name
    -- | Used a return statement in a procedure.
    | ResultInProcedure
    -- | The end of an operator was reached without finding a return statement.
    | ExpectedResult
    -- | Use an operator out of a list definition or a procedure call.
    | ForbiddenIteratorUsed
    -- | An error thrown during the evaluation of a function that can be recovered from.
    | CodeError [String]
    | ParseError String
    deriving (Eq, Show)

data Error = Error (Maybe Location) ErrorType
    deriving (Eq, Show)

data WarningType =
    -- | A value can be understood in more than one way.
    AmbiguousValue Int [Annotated MatchablePart]
    -- | A sentence can be understood in more than one way.
    | AmbiguousSentence Int [Annotated MatchablePart]
    deriving (Eq, Show)

data Warning = Warning (Maybe Location) WarningType
    deriving (Eq, Show)
