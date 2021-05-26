module Errors where

import AST

--


--

data ErrorType =
    WrongTypeValue Type Type
    | WrongTypeParameter Type Type Name
    | UnmatchableValue [Annotated MatchablePart]
    | UnmatchableSentence [Annotated MatchablePart]
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
    deriving (Eq, Show)

data Error = Error (Maybe Location) ErrorType
    deriving (Eq, Show)

--
