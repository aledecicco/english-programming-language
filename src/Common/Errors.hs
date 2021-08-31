{-|
Module      : Errors
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Errors, warnings, and their types.
-}

module Errors where

import           AST


data ErrorType =
    WrongTypeValue Type Type -- ^ Expected a value to have a different type.
    | WrongTypeParameter Type Type Int FunId -- ^ Expected a function parameter to have a different type.
    | UnmatchableValue [Annotated MatchablePart] -- ^ Couldn't understand a list of matchable parts as any value.
    | UnmatchableValueTypes [Annotated MatchablePart] -- ^ A list of matchable parts could be understood as many different values but none of them passed the validations.
    | UnmatchableSentence [Annotated MatchablePart] -- ^ Couldn't understand a list of matchable parts as any sentence.
    | UnmatchableSentenceTypes [Annotated MatchablePart] -- ^ A list of matchable parts could be understood as many different sentences but none of them passed the validations.
    | FunctionAlreadyDefined FunId
    | UndefinedFunction FunId
    | VariableAlreadyDefined Name
    | UndefinedVariable Name
    | MismatchingTypeAssigned Type Type Name -- ^ Assigned a value to a variable of a different type.
    | ResultInProcedure -- ^ Used a return statement in a procedure.
    | ExpectedResult -- ^ The end of an operator was reached without finding a return statement.
    | ForbiddenIteratorUsed -- ^ Use an operator out of a list definition or a procedure call.
    | CodeError [String] -- ^ An error thrown during the evaluation of a function that can be recovered from.
    | ParseError String
    deriving (Eq, Show)

data Error = Error (Maybe Location) ErrorType
    deriving (Eq, Show)

data WarningType =
    AmbiguousValue Int [Annotated MatchablePart] -- ^ A value can be understood in more than one way.
    | AmbiguousSentence Int [Annotated MatchablePart] -- ^ A sentence can be understood in more than one way.
    deriving (Eq, Show)

data Warning = Warning (Maybe Location) WarningType
    deriving (Eq, Show)
