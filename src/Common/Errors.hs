module Errors where

import Control.Monad.Trans.Except ( throwE )
import Control.Monad.Trans.Class ( lift )

import Env
import AST
import PrettyPrinter

--


-- Auxiliary

customError :: Monad m => [String] -> Env a b m r
customError e = do
    ln <- getLineNumber
    lift . throwE $ unwords ["Error in line", show ln, ":\n", unwords e, "\n"]

--


-- Errors

wrongTypeValueError :: Monad m => Type -> Type -> Env a b m r
wrongTypeValueError t t' = customError ["Expected a", ppType t False, "but got a", ppType t' False, "instead"]

wrongTypeParameterError :: Monad m => Type -> Type -> Name -> Env a b m r
wrongTypeParameterError t t' n = customError ["Parameter", doubleQuote $ ppName n, "expected a", ppType t False, "but got a", ppType t' False, "instead"]

unmatchableValueError :: Monad m => [MatchablePart] -> Env a b m r
unmatchableValueError ps = customError ["Could not understand", doubleQuote $ ppMatchable ps, "as a value"]

unmatchableSentenceError :: Monad m => [MatchablePart] -> Env a b m r
unmatchableSentenceError ps = customError ["Could not understand", doubleQuote $ ppMatchable ps, "as a sentence"]

alreadyDefinedFunctionError :: Monad m => FunId -> Env a b m r
alreadyDefinedFunctionError fid = customError ["Funcion", doubleQuote $ ppFunctionId fid, "is already defined"]

functionNotDefinedError :: Monad m => FunId -> Env a b m r
functionNotDefinedError fid = customError ["Function", doubleQuote $ ppFunctionId fid, "is not defined"]

alreadyDefinedVariableError :: Monad m => Name -> Env a b m r
alreadyDefinedVariableError n = customError ["Expected variable", doubleQuote $ ppName n, "to be new but it was already defined"]

undefinedVariableError :: Monad m => Name -> Env a b m r
undefinedVariableError n = customError ["Variable", doubleQuote $ ppName n, "is not defined"]

mismatchingTypeAssignedError :: Monad m => Name -> Type -> Type -> Env a b m r
mismatchingTypeAssignedError n t t' = customError ["Could not assign a", ppType t False, "to variable", doubleQuote $ ppName n, "which is a", ppType t False]

resultInProcedureError :: Monad m => Env a b m r
resultInProcedureError = customError ["Found unexpected result statement in procedure"]

expectedResultError :: Monad m => Env a b m r
expectedResultError = customError ["Expected a result statement before end of operator"]

emptyListError :: Monad m => Env a b m r
emptyListError = customError ["Expected a list with at least one element"]

outOfBoundsIndexError :: Monad m => Int -> Env a b m r
outOfBoundsIndexError n = customError ["Tried to access a list at index", show n, ", which is out of bounds"]

divisionByZeroError :: Monad m => Env a b m r
divisionByZeroError = customError ["Division by zero"]

--
