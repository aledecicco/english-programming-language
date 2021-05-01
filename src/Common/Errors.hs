module Errors where

import Location
import AST
import PrettyPrinter

--


-- Errors

wrongTypeValueError :: Type -> Type -> [String]
wrongTypeValueError t t' = ["Expected a", ppType t False, "but got a", ppType t' False, "instead"]

wrongTypeParameterError :: Type -> Type -> Name -> [String]
wrongTypeParameterError t t' n = ["Parameter", doubleQuote $ ppName n, "expected a", ppType t False, "but got a", ppType t' False, "instead"]

unmatchableValueError :: [MatchablePart a] -> [String]
unmatchableValueError ps = ["Could not understand", doubleQuote $ ppMatchable ps, "as a value"]

unmatchableSentenceError :: [MatchablePart a] -> [String]
unmatchableSentenceError ps = ["Could not understand", doubleQuote $ ppMatchable ps, "as a sentence"]

alreadyDefinedFunctionError :: FunId -> [String]
alreadyDefinedFunctionError fid = ["Funcion", doubleQuote $ ppFunctionId fid, "is already defined"]

functionNotDefinedError :: FunId -> [String]
functionNotDefinedError fid = ["Function", doubleQuote $ ppFunctionId fid, "is not defined"]

alreadyDefinedVariableError :: Name -> [String]
alreadyDefinedVariableError n = ["Expected variable", doubleQuote $ ppName n, "to be new but it was already defined"]

undefinedVariableError :: Name -> [String]
undefinedVariableError n = ["Variable", doubleQuote $ ppName n, "is not defined"]

mismatchingTypeAssignedError :: Name -> Type -> Type -> [String]
mismatchingTypeAssignedError n t t' = ["Could not assign a", ppType t False, "to variable", doubleQuote $ ppName n, "which is a", ppType t False]

resultInProcedureError :: [String]
resultInProcedureError = ["Found unexpected result statement in procedure"]

expectedResultError :: [String]
expectedResultError = ["Expected a result statement before end of operator"]

emptyListError :: [String]
emptyListError = ["Expected a list with at least one element"]

outOfBoundsIndexError :: Int -> [String]
outOfBoundsIndexError n = ["Tried to access a list at index", show n, ", which is out of bounds"]

divisionByZeroError :: [String]
divisionByZeroError = ["Division by zero"]

--
