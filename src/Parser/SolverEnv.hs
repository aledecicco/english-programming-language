module SolverEnv ( module SolverEnv, withLocation, initialLocation, getLocation, getFirstLocation ) where

import Data.List ( find )
import Data.Bifunctor ( first, second )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( get, gets, modify, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, runExcept, Except )

import Errors
import Location
import AST

--


-- Type definition

type SolverData = ([(FunId, FunSignature)], [(Name, Type)])
type SolverEnv a = LocationT (StateT SolverData (Except Error)) a

runSolverEnv :: SolverEnv a -> SolverData -> Location -> Either Error ((a, Location), SolverData)
runSolverEnv f d s = runExcept $ runStateT (runLocationT f s) d

initialState :: SolverData
initialState = ([], [])

--


-- Errors

throw :: ErrorType -> SolverEnv a
throw eT = lift . lift . throwE $ Error Nothing eT

throwHere :: ErrorType -> SolverEnv a
throwHere eT = do
    l <- getCurrentLocation
    lift . lift . throwE $ Error (Just l) eT

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

resetVariables :: SolverEnv ()
resetVariables = changeVariables $ const []

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

getOperatorSignatures :: SolverEnv [FunSignature]
getOperatorSignatures = do
    fs <- fst <$> lift get
    return $ [f | (_, f@(FunSignature _ (Operator _))) <- fs]

getProcedureSignatures :: SolverEnv [FunSignature]
getProcedureSignatures = do
    fs <- fst <$> lift get
    return $ [f | (_, f@(FunSignature _ Procedure)) <- fs]

functionIsDefined :: FunId -> SolverEnv Bool
functionIsDefined fid = do
    r <- getFunctionSignature fid
    case r of
        Just _ -> return True
        _ -> return False

setFunctions :: [(FunId, FunSignature)] -> SolverEnv ()
setFunctions = changeFunctions . const

--
