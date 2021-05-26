module ParserEnv ( module ParserEnv, withLocation, initialLocation, getLocation, getFirstLocation ) where

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

type ParserData = ([(FunId, FunSignature)], [(Name, Type)])
type ParserEnv a = LocationT (StateT ParserData (Except Error)) a

runParserEnv :: ParserEnv a -> ParserData -> Location -> Either Error ((a, Location), ParserData)
runParserEnv f d s = runExcept $ runStateT (runLocationT f s) d

initialState :: ParserData
initialState = ([], [])

--


-- Errors

throw :: ErrorType -> ParserEnv a
throw eT = lift . lift . throwE $ Error Nothing eT

throwHere :: ErrorType -> ParserEnv a
throwHere eT = do
    l <- getCurrentLocation
    lift . lift . throwE $ Error (Just l) eT

--


-- Auxiliary

changeFunctions :: ([(FunId, FunSignature)] -> [(FunId, FunSignature)]) -> ParserEnv ()
changeFunctions m = lift $ modify (first m)

changeVariables :: ([(Name, Type)] -> [(Name, Type)]) -> ParserEnv ()
changeVariables m = lift $ modify (second m)


--


-- Variables

getVariableType :: Name -> ParserEnv (Maybe Type)
getVariableType vn = lift $ gets (lookup vn . snd)

setVariableType :: Name -> Type -> ParserEnv ()
setVariableType vn t = do
    removeVariableType vn
    lift $ modify (second ((vn, t):))

removeVariableType :: Name -> ParserEnv ()
removeVariableType vn =
    let removeVn = filter (\vd -> fst vd /= vn)
    in changeVariables removeVn

variableIsDefined :: Name -> ParserEnv Bool
variableIsDefined vn = do
    r <- getVariableType vn
    case r of
        Just _ -> return True
        _ -> return False

resetVariables :: ParserEnv ()
resetVariables = changeVariables $ const []

--


-- Functions

getFunctionSignature :: FunId -> ParserEnv (Maybe FunSignature)
getFunctionSignature fid = lift $ gets (lookup fid .fst)

setFunctionSignature :: FunId -> FunSignature -> ParserEnv ()
setFunctionSignature fid s = do
    removeFunctionSignature fid
    changeFunctions ((fid, s):)

removeFunctionSignature :: FunId -> ParserEnv ()
removeFunctionSignature fid =
    let removeFid = filter (\fd -> fst fd /= fid)
    in changeFunctions removeFid

getOperatorSignatures :: ParserEnv [FunSignature]
getOperatorSignatures = do
    fs <- fst <$> lift get
    return $ [f | (_, f@(FunSignature _ (Operator _))) <- fs]

getProcedureSignatures :: ParserEnv [FunSignature]
getProcedureSignatures = do
    fs <- fst <$> lift get
    return $ [f | (_, f@(FunSignature _ Procedure)) <- fs]

functionIsDefined :: FunId -> ParserEnv Bool
functionIsDefined fid = do
    r <- getFunctionSignature fid
    case r of
        Just _ -> return True
        _ -> return False

setFunctions :: [(FunId, FunSignature)] -> ParserEnv ()
setFunctions = changeFunctions . const

--
