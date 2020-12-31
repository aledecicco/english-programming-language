module Matcher where

import Control.Monad ( when, void )
import Control.Monad.Trans.State ( get, gets, put, modify, liftCatch, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, catchE, runExcept, Except )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find )

import Types
import PreludeDefs

--


-- State definitions

type Error = String

type FunEnv = [Function]
type VarEnv = [(Name, Type)]
type RecEnv = [(Name, [(Name, Type)])]
type Env = (FunEnv, VarEnv, RecEnv)

type MatcherState a = StateT Env (Except Error) a

-- Tries the first action and, if it fails, reverts the state and tries the second action
(<|>) :: MatcherState a -> MatcherState a -> MatcherState a
ma <|> mb = liftCatch catchE ma (const mb)

-- Tries each action in a list until one of them succeeds
try :: [MatcherState a] -> MatcherState a
try = foldr (<|>) (error "No options left in \"try\"")

initialEnv :: Env
initialEnv = (operators ++ procedures, [], [])

getVarType :: Name -> MatcherState (Maybe Type)
getVarType vn = do
    vars <- gets (\(_, vE, _) -> vE)
    let r = find (\vd -> fst vd == vn) vars
    return $ snd <$> r

setVarType :: Name -> Type -> MatcherState ()
setVarType vn t' = do
    r <- getVarType vn
    case r of
        Just t -> when (t /= t') $ missmatchingTypeAssign vn t t'
        Nothing -> modify (\(fE, vE, rE) -> (fE, (vn, t'):vE, rE))

resetVariables :: MatcherState ()
resetVariables = modify (\(fE, _, rE) -> (fE, [], rE))

--


-- Errors

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
missmatchingTypeAssign :: Name -> Type -> Type -> MatcherState a
missmatchingTypeAssign vn t t' = lift . throwE $ "Can't assign value of type " ++ show t' ++ " to variable " ++ concat vn ++ " with type " ++ show t

-- Error that occurs when a matchable can't be matched
unmatchable :: [MatchablePart] -> MatcherState a
unmatchable ps = lift . throwE $ "\"" ++ show ps ++ "\" couldn't be matched"

--


-- Matchers

matchBlock :: Block -> MatcherState Block
matchBlock = return


--


-- Main

-- Returns the given program with its matchables matched
matchProgram :: Program -> Either Error (Program, Env)
matchProgram p = runExcept $ runStateT (return []) initialEnv

--
