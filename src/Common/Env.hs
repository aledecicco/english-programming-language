module Env where

import Control.Monad.Trans.State ( gets, modify, StateT )
import Control.Monad.Trans.Except ( Except )
import Data.Bifunctor ( first, second )
import Data.List ( find, intercalate )

import AST

--


-- Type definitions

type Error = String

type FunEnv a = [(FunctionId, a)]
type VarEnv a = [(Name, a)]

type Env a b r = StateT (FunEnv a, VarEnv b, LineNumber) (Except Error) r

--


-- Env handlers

getVarEnv :: Env a b (VarEnv b)
getVarEnv = gets (\(_, vE, _) -> vE)

setVarEnv :: VarEnv b -> Env a b ()
setVarEnv vE = modify (\(fE, _, ln) -> (fE, vE, ln))

getFunEnv :: Env a b (FunEnv a)
getFunEnv = gets (\(fE, _, _) -> fE)

setFunEnv :: FunEnv a -> Env a b ()
setFunEnv fE = modify (\(_, vE, ln) -> (fE, vE, ln))

getLineNumber :: Env a b LineNumber
getLineNumber = gets (\(_, _, ln) -> ln)

setLineNumber :: LineNumber -> Env a b ()
setLineNumber ln = modify (\(fE, vE, _) -> (fE, vE, ln))

--


-- Variables

getVariable :: Name -> Env a b (Maybe b)
getVariable vn = do
    vE <- getVarEnv
    let r = find (\vd -> fst vd == vn) vE
    return $ snd <$> r

setVariable :: Name -> b -> Env a b ()
setVariable vn t' = do
    removeVariable vn
    vE <- getVarEnv
    setVarEnv $ (vn, t'):vE

removeVariable :: Name -> Env a b ()
removeVariable vn = do
    vE <- getVarEnv
    let vE' = filter ((vn /=) . fst) vE
    setVarEnv vE'

variableIsDefined :: Name -> Env a b Bool
variableIsDefined vn = do
    r <- getVariable vn
    case r of
        Just _ -> return True
        Nothing -> return False

resetVariables :: Env a b ()
resetVariables = setVarEnv []

--


-- Functions

getFunction :: FunctionId -> Env a b (Maybe a)
getFunction fid = do
    r <- find (\(fid', _) -> fid == fid') <$> getFunEnv
    case r of
        Just (_, f) -> return $ Just f
        Nothing -> return Nothing

setFunction :: FunctionId -> a -> Env a b ()
setFunction fid f = do
    removeFunction fid
    fE <- getFunEnv
    setFunEnv $ (fid, f):fE

functionIsDefined :: FunctionId -> Env a b Bool
functionIsDefined fid = do
    r <- getFunction fid
    case r of
        Just _ -> return True
        Nothing -> return False

removeFunction :: FunctionId -> Env a b ()
removeFunction fid = do
    fE <- getFunEnv
    let fE' = filter ((fid /=) . fst) fE
    setFunEnv fE'

--
