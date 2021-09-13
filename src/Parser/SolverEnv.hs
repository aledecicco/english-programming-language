{-|
Module      : SolverEnv
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The monad on which the "Solver" runs, with some useful operations.
-}
module SolverEnv (module SolverEnv, module Location, catchError) where

import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, ExceptT)
import Control.Monad.Trans.State (gets, modify, runStateT, runState, State, StateT)
import Data.Bifunctor (first, second)
import Data.Maybe (isJust)
import qualified Data.Map.Strict as M

import AST
import BuiltInDefs (builtInFunctions)
import Errors
import Location


-- -----------------
-- * Type definitions

-- | A mapping between function ids and their signatures.
-- The use of a list instead of a map is intentional, since the order of definition of each function affects its precedence.
type FunData = [(FunId, FunSignature)]

-- | A mapping between variable names and their types.
type VarData = M.Map Name Type

-- | The state of the solver.
type SolverData = (FunData, VarData)

-- | The solver's monad.
-- It stores the solver's state.
-- It has the ability to issue warnings and throw and catch errors.
-- It stores the current location.
type SolverEnv a = LocationT (StateT SolverData (ExceptT Error (State [Warning]))) a

runSolverEnv :: SolverEnv a -> [Warning] -> Location -> SolverData -> (Either Error ((a, Location), SolverData), [Warning])
runSolverEnv action warnings location state = runState (runExceptT $ runStateT (runLocationT action location) state) warnings

-- | The default initial state of the solver, including the built-in functions and no variables.
initialState :: SolverData
initialState = (builtInFunctions, M.empty)


-- -----------------
-- * Errors

-- | Throw an error of the given type without specifiying a location.
throwNowhere :: ErrorType -> SolverEnv a
throwNowhere errType = throwError $ Error Nothing errType

-- | Throw an error of the given type at the current location.
throwHere :: ErrorType -> SolverEnv a
throwHere errType = do
    location <- getCurrentLocation
    throwError $ Error (Just location) errType

-- | Issue a warning of the given type at the current location.
warnHere :: WarningType -> SolverEnv ()
warnHere warnType = do
    location <- getCurrentLocation
    lift .lift . lift $ modify (Warning (Just location) warnType :)


-- -----------------
-- * Operations on functions

-- | Modifies the state through a 'FunData' modifier.
modifyFunctions :: (FunData -> FunData) -> SolverEnv ()
modifyFunctions modF = lift $ modify (first modF)

getFunctions :: SolverEnv FunData
getFunctions = lift $ gets fst

-- | Returns the 'FunSignature' of a function, assuming it is defined.
getFunctionSignature :: FunId -> SolverEnv FunSignature
getFunctionSignature fid = do
    res <- lookup fid <$> getFunctions
    case res of
        Just sgn -> return sgn
        Nothing -> error $ "Function " ++ show fid ++ " is not defined"

-- | Sets the 'FunSignature' of a function, assuming it is not defined.
setFunctionSignature :: FunId -> FunSignature -> SolverEnv ()
setFunctionSignature fid sgn = modifyFunctions ((fid, sgn):)

functionIsDefined :: FunId -> SolverEnv Bool
functionIsDefined fid = isJust . lookup fid <$> getFunctions

getOperatorSignatures :: SolverEnv [FunSignature]
getOperatorSignatures = do
    funs <- getFunctions
    return [fun | (_, fun@(FunSignature _ (Operator _))) <- funs]

getProcedureSignatures :: SolverEnv [FunSignature]
getProcedureSignatures = do
    funs <- getFunctions
    return [fun | (_, fun@(FunSignature _ Procedure)) <- funs]


-- -----------------
-- * Operations on variables

-- | Modifies the state through a 'VarData' modifier.
modifyVariables :: (VarData -> VarData) -> SolverEnv ()
modifyVariables modF = lift $ modify (second modF)

getVariables :: SolverEnv VarData
getVariables = lift $ gets snd

-- | Returns the type of a variable, assuming it is defined.
getVariableType :: Name -> SolverEnv Type
getVariableType name = (M.! name) <$> getVariables

-- | Sets the type of a variable, assuming it is not defined.
setVariableType :: Name -> Type -> SolverEnv ()
setVariableType name varType = modifyVariables (M.insert name varType)

removeVariableType :: Name -> SolverEnv ()
removeVariableType name = modifyVariables (M.delete name)

variableIsDefined :: Name -> SolverEnv Bool
variableIsDefined name = M.member name <$> getVariables

-- | Run a computation and reset the variables to how they where before running it.
restoringVariables :: SolverEnv a -> SolverEnv a
restoringVariables action = do
    vars <- getVariables
    res <- action
    modifyVariables $ const vars
    return res
