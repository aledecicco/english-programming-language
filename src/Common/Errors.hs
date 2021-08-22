module Errors where

import AST

--


--

data ErrorType =
    WrongTypeValue Type Type
    | WrongTypeParameter Type Type Int FunId
    | UnmatchableValue [Annotated MatchablePart]
    | UnmatchableValueTypes [Annotated MatchablePart]
    | UnmatchableSentence [Annotated MatchablePart]
    | UnmatchableSentenceTypes [Annotated MatchablePart]
    | FunctionAlreadyDefined FunId
    | UndefinedFunction FunId
    | VariableAlreadyDefined Name
    | UndefinedVariable Name
    | MismatchingTypeAssigned Type Type Name
    | ResultInProcedure
    | ExpectedResult
    | EmptyList
    | OutOfBoundsIndex Int
    | DivisionByZero
    | ForbiddenIteratorUsed
    deriving (Eq, Show)

data Error = Error (Maybe Location) ErrorType
    deriving (Eq, Show)

--
