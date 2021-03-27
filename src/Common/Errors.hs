module Errors where

import Control.Monad.Trans.Except ( throwE )
import Control.Monad.Trans.Class ( lift )

import Env
import AST

--


-- Auxiliary

quote :: Show a => a -> String
quote x = "\"" ++ show x ++ "\""

customError :: Monad m => [String] -> Env a b m r
customError e = do
    ln <- getLineNumber
    lift . throwE $ unwords ["Error in line", show ln, ":\n", unwords e, "\n"]

--


-- Errors

wrongTypeValueError :: Monad m => Value -> Type -> Env a b m r
wrongTypeValueError v t = customError ["Expected value of type", quote t, ", but got", quote v, "instead"]

wrongTypeParameterError :: Monad m => Value -> Type -> Name -> Env a b m r
wrongTypeParameterError v t n = customError ["Parameter", quote n, "expeceted value of type", quote t, ", but got", quote v, "instead"]

unmatchableValueError :: Monad m => [MatchablePart] -> Env a b m r
unmatchableValueError ps = customError ["Could not understand", quote ps, "as a value"]

unmatchableSentenceError :: Monad m => [MatchablePart] -> Env a b m r
unmatchableSentenceError ps = customError ["Could not understand", quote ps, "as a sentence"]

alreadyDefinedFunctionError :: Monad m => Title -> Env a b m r
alreadyDefinedFunctionError t = customError ["Funcion", quote t, "is already defined"]

alreadyDefinedVariableError :: Monad m => Name -> Env a b m r
alreadyDefinedVariableError n = customError ["Expected variable", quote n, "to be new but it was already defined"]

undefinedVariableError :: Monad m => Name -> Env a b m r
undefinedVariableError n = customError ["Variable", quote n, "is not defined"]

mismatchingTypeAssignedError :: Monad m => Name -> Type -> Type -> Env a b m r
mismatchingTypeAssignedError n t t' = customError ["Could not assign value of type", quote t, "to variable", show n, "of type", quote t']

resultInProcedureError :: Monad m => Env a b m r
resultInProcedureError = customError ["Found unexpected result statement in procedure"]

expectedResultError :: Monad m => Env a b m r
expectedResultError = customError ["Expected a result statement before end of operator"]

emptyListError :: Monad m => Env a b m r
emptyListError = customError ["Expected a list with at least one element"]

outOfBoundsIndexError :: Monad m => Int -> Env a b m r
outOfBoundsIndexError n = customError ["Tried to access a list at index", show n, ", which is out of bounds"]

--
