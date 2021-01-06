module Matcher where

import Control.Monad ( when, void )
import Control.Monad.Trans.State ( get, gets, put, modify, liftCatch, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, catchE, runExcept, Except )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find )

import Types
import PreludeDefs

--


-- Environment

type Error = String

type FunEnv = [Function]
type VarEnv = [(Name, Type)]
type RecEnv = [(Name, [(Name, Type)])]
type Env = (FunEnv, VarEnv, RecEnv)

type MatcherState a = StateT Env (Except Error) a

initialEnv :: Env
initialEnv = (operators ++ procedures, [], [])

getVarEnv :: MatcherState VarEnv
getVarEnv = gets (\(_, vE, _) -> vE)

setVarEnv :: VarEnv -> MatcherState ()
setVarEnv vE = modify (\(fE, _, rE) -> (fE, vE, rE))

getVarType :: Name -> MatcherState (Maybe Type)
getVarType vn = do
    vE <- getVarEnv
    let r = find (\vd -> fst vd == vn) vE
    return $ snd <$> r

setVarType :: Name -> Type -> MatcherState ()
setVarType vn t' = do
    r <- getVarType vn
    case r of
        Just t -> when (t /= t') $ missmatchingTypeAssign vn t t'
        Nothing -> modify (\(fE, vE, rE) -> (fE, (vn, t'):vE, rE))

varIsDefined :: Name -> MatcherState Bool
varIsDefined vn = do
    r <- getVarType vn
    case r of
        Just _ -> return True
        Nothing -> return False

-- Tries the first action and, if it fails, reverts the state and tries the second action
(<|>) :: MatcherState a -> MatcherState a -> MatcherState a
ma <|> mb = liftCatch catchE ma (const mb)

-- Tries each action in a list until one of them succeeds
try :: [MatcherState a] -> String -> MatcherState a
try actions msg = foldr (<|>) (customError msg) actions

-- Empties the variables environment
resetVariables :: MatcherState ()
resetVariables = setVarEnv []

--


-- Errors

customError :: String -> MatcherState a
customError = lift . throwE

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
missmatchingTypeAssign :: Name -> Type -> Type -> MatcherState a
missmatchingTypeAssign vn t t' = customError $ "Can't assign value of type " ++ show t' ++ " to variable " ++ concat vn ++ " with type " ++ show t

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
alreadyDefinedArgument :: Name -> MatcherState a
alreadyDefinedArgument vn = customError $ "Argument name " ++ concat vn ++ " can't be used more than once"

-- Error that occurs when a matchable can't be matched
unmatchable :: [MatchablePart] -> MatcherState a
unmatchable ps = customError $ "\"" ++ show ps ++ "\" couldn't be matched"

--


-- Auxiliary

registerTitleParameters :: Title -> MatcherState ()
registerTitleParameters [] = return ()
registerTitleParameters (t:ts) = do
    case t of (TitleParam n t) -> registerParameter n t
    registerTitleParameters ts
    where
        registerParameter :: Name -> Type -> MatcherState ()
        registerParameter n t = do
            r <- varIsDefined n
            if r then alreadyDefinedArgument n else setVarType n t

commitSentence :: Sentence -> MatcherState ()
commitSentence (VarDef ns v) = return ()
commitSentence (If v s) = return ()
commitSentence (IfElse v sTrue sFalse) = return ()
commitSentence (For n vFrom vTo ss) = return ()
commitSentence (ForEach n v ss) = return ()
commitSentence (Until v ss) = return ()
commitSentence (While v ss) = return ()
commitSentence (Result t v) = return ()
commitSentence (ProcedureCall t vs) = return ()

--


-- Matchers

matchAsProcedureCall :: [MatchablePart] -> MatcherState [Sentence]
matchAsProcedureCall ps = return []

matchSentence :: Sentence -> MatcherState [Sentence]
matchSentence (SentenceM ps) = matchAsProcedureCall ps
matchSentence s = return [s]

matchSentences :: [Sentence] -> MatcherState [Sentence]
matchSentences [] = return []
matchSentences (s:rest) = do
    ss <- matchSentence s
    try (map trySentence ss) "Couldn't match sentence"
    where
        trySentence s' = do
            commitSentence s'
            rest' <- matchSentences rest
            return $ s':rest'

matchBlock :: Block -> MatcherState Block
matchBlock b = case b of
    StructDef {} -> return b
    FunDef t ss -> do
        registerTitleParameters t
        ss' <- matchSentences ss
        return $ FunDef t ss'

matchBlocks :: Program -> MatcherState Program
matchBlocks [] = return []
matchBlocks (b:bs) = do
    b' <- matchBlock b
    resetVariables
    bs' <- matchBlocks bs
    return $ b':bs'



--


-- Main

-- Returns the given program with its matchables matched
matchProgram :: Program -> Either Error (Program, Env)
matchProgram p = runExcept $ runStateT matchProgram' initialEnv
    where
        matchProgram' = do
            p1 <- registerStructs p
            p2 <- matchTitles p1
            p3 <- registerFunctions p2
            matchBlocks p3

--
