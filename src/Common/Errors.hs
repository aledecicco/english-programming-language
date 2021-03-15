module Errors where

import Control.Monad.Trans.Except ( throwE )
import Control.Monad.Trans.Class ( lift )

import Env
import AST

--


-- Auxiliary

quote :: Show a => a -> String
quote x = "\"" ++ show x ++ "\""

customError :: [String] -> Env a b r
customError e = do
    ln <- getLineNumber
    lift . throwE $ unwords ["Error in line", show ln, ":\n", unwords e, "\n"]

--


-- Errors

wrongTypeValueError :: Value -> Type -> Env a b r
wrongTypeValueError v t = customError ["Expected value of type", quote t, ", but got", quote v, "instead"]

wrongTypeParameterError :: Value -> Type -> Name -> Env a b r
wrongTypeParameterError v t n = customError ["Parameter", quote n, "expeceted value of type", quote t, ", but got", quote v, "instead"]

unmatchableValueError :: [MatchablePart] -> Env a b r
unmatchableValueError ps = customError ["Could not understand", quote ps, "as a value"]

unmatchableSentenceError :: [MatchablePart] -> Env a b r
unmatchableSentenceError ps = customError ["Could not understand", quote ps, "as a sentence"]

alreadyDefinedVariableError :: Name -> Env a b r
alreadyDefinedVariableError n = customError ["Expected variable", quote n, "to be new but it was already defined"]

mismatchingTypeAssignedError :: Name -> Type -> Type -> Env a b r
mismatchingTypeAssignedError n t t' = customError ["Could not assign value of type", quote t, "to variable", show n, "of type", quote t']

resultInProcedureError :: Env a b r
resultInProcedureError = customError ["Found unexpected return statement in procedure"]
