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
    | ForbiddenIteratorUsed
    | CodeError [String]
    | ParseError String
    deriving (Eq, Show)

data Error = Error (Maybe Location) ErrorType
    deriving (Eq, Show)

data WarningType =
    AmbiguousValue Int [Annotated MatchablePart]
    | AmbiguousSentence Int [Annotated MatchablePart]
    deriving (Eq, Show)

data Warning = Warning (Maybe Location) WarningType
    deriving (Eq, Show)

--
