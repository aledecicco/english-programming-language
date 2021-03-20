module Env where

import Control.Monad.Trans.State ( gets, modify, StateT )
import Control.Monad.Trans.Except ( ExceptT )
import Data.Bifunctor ( first, second )
import Data.List ( find, intercalate )

import AST

--


-- Type definitions

type Error = String

type FunEnv a = [(FunctionId, a)]
type VarEnv a = [(Name, a)]
type EnvData a b = (FunEnv a, VarEnv b, LineNumber)

type Env a b m r = StateT (EnvData a b) (ExceptT Error m) r

--


-- Env handlers

getVarEnv :: Monad m => Env a b m (VarEnv b)
getVarEnv = gets (\(_, vE, _) -> vE)

setVarEnv :: Monad m => VarEnv b -> Env a b m ()
setVarEnv vE = modify (\(fE, _, ln) -> (fE, vE, ln))

getFunEnv :: Monad m => Env a b m (FunEnv a)
getFunEnv = gets (\(fE, _, _) -> fE)

setFunEnv :: Monad m => FunEnv a -> Env a b m ()
setFunEnv fE = modify (\(_, vE, ln) -> (fE, vE, ln))

getLineNumber :: Monad m => Env a b m LineNumber
getLineNumber = gets (\(_, _, ln) -> ln)

setLineNumber :: Monad m => LineNumber -> Env a b m ()
setLineNumber ln = modify (\(fE, vE, _) -> (fE, vE, ln))

--


-- Variables

getVariable :: Monad m => Name -> Env a b m (Maybe b)
getVariable vn = do
    vE <- getVarEnv
    let r = find (\vd -> fst vd == vn) vE
    return $ snd <$> r

setVariable :: Monad m => Name -> b -> Env a b m ()
setVariable vn t' = do
    removeVariable vn
    vE <- getVarEnv
    setVarEnv $ (vn, t'):vE

removeVariable :: Monad m => Name -> Env a b m ()
removeVariable vn = do
    vE <- getVarEnv
    let vE' = filter ((vn /=) . fst) vE
    setVarEnv vE'

variableIsDefined :: Monad m => Name -> Env a b m Bool
variableIsDefined vn = do
    r <- getVariable vn
    case r of
        Just _ -> return True
        Nothing -> return False

resetVariables :: Monad m => Env a b m ()
resetVariables = setVarEnv []

--


-- Functions

getFunction :: Monad m => FunctionId -> Env a b m (Maybe a)
getFunction fid = do
    r <- find (\(fid', _) -> fid == fid') <$> getFunEnv
    case r of
        Just (_, f) -> return $ Just f
        Nothing -> return Nothing

setFunction :: Monad m => FunctionId -> a -> Env a b m ()
setFunction fid f = do
    removeFunction fid
    fE <- getFunEnv
    setFunEnv $ (fid, f):fE

functionIsDefined :: Monad m => FunctionId -> Env a b m Bool
functionIsDefined fid = do
    r <- getFunction fid
    case r of
        Just _ -> return True
        Nothing -> return False

removeFunction :: Monad m => FunctionId -> Env a b m ()
removeFunction fid = do
    fE <- getFunEnv
    let fE' = filter ((fid /=) . fst) fE
    setFunEnv fE'

--
