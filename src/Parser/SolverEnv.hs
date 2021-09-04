module SolverEnv ( module SolverEnv, module Location, catchError ) where

import Data.Bifunctor ( first, second )
import Control.Monad.Except ( throwError, catchError )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( gets, modify, runStateT, runState, State, StateT )
import Control.Monad.Trans.Except ( runExceptT, ExceptT )

import BuiltInDefs (builtInFunctions)
import Errors
import Location
import AST

--


-- Type definition

type SolverData = ([(FunId, FunSignature)], [(Name, Type)])
type SolverEnv a = LocationT (StateT SolverData (ExceptT Error (State [Warning]))) a

runSolverEnv :: SolverEnv a -> [Warning] -> Location -> SolverData -> (Either Error ((a, Location), SolverData), [Warning])
runSolverEnv f w l d = runState (runExceptT $ runStateT (runLocationT f l) d) w

initialState :: SolverData
initialState = (builtInFunctions, [])

--


-- Errors

throwNowhere :: ErrorType -> SolverEnv a
throwNowhere eT = throwError $ Error Nothing eT

throwHere :: ErrorType -> SolverEnv a
throwHere eT = do
    l <- getCurrentLocation
    throwError $ Error (Just l) eT

warnHere :: WarningType -> SolverEnv ()
warnHere wT = do
    l <- getCurrentLocation
    lift .lift . lift $ modify (Warning (Just l) wT :)

--


-- Auxiliary

changeFunctions :: ([(FunId, FunSignature)] -> [(FunId, FunSignature)]) -> SolverEnv ()
changeFunctions m = lift $ modify (first m)

changeVariables :: ([(Name, Type)] -> [(Name, Type)]) -> SolverEnv ()
changeVariables m = lift $ modify (second m)

--


-- Variables

getVariableType :: Name -> SolverEnv (Maybe Type)
getVariableType vn = lift $ gets (lookup vn . snd)

setVariableType :: Name -> Type -> SolverEnv ()
setVariableType vn t = do
    removeVariableType vn
    lift $ modify (second ((vn, t):))

removeVariableType :: Name -> SolverEnv ()
removeVariableType vn =
    let removeVn = filter (\vd -> fst vd /= vn)
    in changeVariables removeVn

variableIsDefined :: Name -> SolverEnv Bool
variableIsDefined vn = do
    r <- getVariableType vn
    case r of
        Just _ -> return True
        _ -> return False

restoringVariables :: SolverEnv a -> SolverEnv a
restoringVariables action = do
    vs <- lift $ gets snd
    r <- action
    changeVariables $ const vs
    return r

--


-- Functions

getFunctionSignature :: FunId -> SolverEnv (Maybe FunSignature)
getFunctionSignature fid = lift $ gets (lookup fid .fst)

setFunctionSignature :: FunId -> FunSignature -> SolverEnv ()
setFunctionSignature fid s = do
    removeFunctionSignature fid
    changeFunctions ((fid, s):)

removeFunctionSignature :: FunId -> SolverEnv ()
removeFunctionSignature fid =
    let removeFid = filter (\fd -> fst fd /= fid)
    in changeFunctions removeFid

functionIsDefined :: FunId -> SolverEnv Bool
functionIsDefined fid = do
    r <- getFunctionSignature fid
    case r of
        Just _ -> return True
        _ -> return False

getOperatorSignatures :: SolverEnv [FunSignature]
getOperatorSignatures = do
    fs <- lift $ gets fst
    return $ [f | (_, f@(FunSignature _ (Operator _))) <- fs]

getProcedureSignatures :: SolverEnv [FunSignature]
getProcedureSignatures = do
    fs <- lift $ gets fst
    return $ [f | (_, f@(FunSignature _ Procedure)) <- fs]

--
