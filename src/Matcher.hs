module Matcher where

import Control.Monad ( when, unless, void, filterM )
import Control.Monad.Trans.State ( get, gets, put, modify, liftCatch, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, catchE, runExcept, Except )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find )
import Data.Maybe ( isJust, isNothing, fromJust )
import Data.Char ( toLower )

import Types
import PreludeDefs

--


-- Environment

type Error = String

type FunEnv = [Function]
type VarEnv = [(Name, Type)]
type Env = (FunEnv, VarEnv)

type MatcherState a = StateT Env (Except Error) a

initialEnv :: Env
initialEnv = (operators ++ procedures, [])

getVarEnv :: MatcherState VarEnv
getVarEnv = gets snd

getFunEnv :: MatcherState FunEnv
getFunEnv = gets fst

setVarEnv :: VarEnv -> MatcherState ()
setVarEnv vE = modify (\(fE, _) -> (fE, vE))

getVarType :: Name -> MatcherState (Maybe Type)
getVarType vn = do
    vE <- getVarEnv
    let r = find (\vd -> fst vd == vn) vE
    return $ snd <$> r

setVarType :: Name -> Type -> MatcherState ()
setVarType vn t' = do
    r <- getVarType vn
    case r of
        Just t -> when (t /= t') $ missmatchingTypeAssignError vn t t'
        Nothing -> modify (\(fE, vE) -> (fE, (vn, t'):vE))

removeVarType :: Name -> MatcherState ()
removeVarType vn = do
    vE <- getVarEnv
    let vE' = filter ((vn /=) . fst) vE
    setVarEnv vE'

getFunction :: Title -> MatcherState (Maybe Function)
getFunction t = find funFinder <$> getFunEnv
    where
        funFinder :: Function -> Bool
        funFinder (Operator t' _) = t == t'
        funFinder (Procedure t') = t == t'

isOperator :: Title -> MatcherState Bool
isOperator t = do
    r <- getFunction t
    case r of
        Just (Operator _ _) -> return True
        _ -> return False

functionIsDefined :: Title -> MatcherState Bool
functionIsDefined t = do
    r <- getFunction t
    case r of
        Just _ -> return True
        Nothing -> return False

-- Returns the resulting type of a call to an operator
getOperatorCallType :: Title -> [Type] -> MatcherState (Maybe Type)
getOperatorCallType fT vTs = do
    f <- getFunction fT
    case f of
        Just (Operator _ tFun) -> return $ Just (tFun vTs)
        _ -> return Nothing

-- Returns the type of an operator assuming it was user defined and hence its types are not polymorphic
getUserDefinedOperatorType :: Title -> MatcherState (Maybe Type)
getUserDefinedOperatorType fT = do
    f <- getFunction fT
    case f of
        Just (Operator _ tFun) -> return $ Just (tFun [])
        _ -> return Nothing

varIsDefined :: Name -> MatcherState Bool
varIsDefined vn = do
    r <- getVarType vn
    case r of
        Just _ -> return True
        Nothing -> return False

-- Tries the first action and, if it fails, reverts the state and tries the second action
(<|>) :: MatcherState a -> MatcherState a -> MatcherState a
ma <|> mb = liftCatch catchE ma (const mb)

-- Empties the variables environment
resetVariables :: MatcherState ()
resetVariables = setVarEnv []

--


-- Errors

customError :: String -> MatcherState a
customError = lift . throwE

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
missmatchingTypeAssignError :: Name -> Type -> Type -> MatcherState a
missmatchingTypeAssignError vn t t' = customError $ "Can't assign value of type " ++ show t' ++ " to variable " ++ concat vn ++ " with type " ++ show t

-- Error that occurs when an undefined variable is used
undefinedVariableError :: Name -> MatcherState a
undefinedVariableError vn = customError $ "Variable \"" ++ concat vn ++ "\" is not defined"

-- Error that occurs when two parameters with the same name are used in a title
alreadyDefinedParameterError :: Name -> MatcherState a
alreadyDefinedParameterError vn = customError $ "Parameter name \"" ++ concat vn ++ "\" can't be used more than once"

-- Error that occurs when a loop uses an iterator with the same name as an already defined variable
alreadyDefinedIteratorError :: Name -> MatcherState a
alreadyDefinedIteratorError vn = customError $ "Variable \"" ++ concat vn ++ "\" is already in use"

-- Error that occurs when a value of the wrong type is used in a sentence
wrongTypeValueError :: Value -> Type -> MatcherState a
wrongTypeValueError v t = customError $ "Expected value of type \"" ++ show t ++ ", but got \"" ++ show v ++ "\" instead"

-- Error that occurs when a value can't be understood
unmatchableValueError :: Value -> MatcherState a
unmatchableValueError v = customError $ "Couldn't understand the value \"" ++ show v ++ "\""

-- Error that occurs when a sentence can't be understood
unmatchableSentenceError :: Sentence -> MatcherState a
unmatchableSentenceError s = customError $ "Couldn't understand the sentence \"" ++ show s ++ "\""

-- Error that occurs when a return statement is used in a procedure
procedureReturnError :: Title -> MatcherState a
procedureReturnError t = customError $ "Function \"" ++ show t ++ "\" can't return a value"

-- Error that occurs when a matchable can't be matched
unmatchableError :: [MatchablePart] -> MatcherState a
unmatchableError ps = customError $ "\"" ++ show ps ++ "\" couldn't be understood"

-- Error that occurs when a matchable can't be matched a specific way
unmatchableAsError :: String -> [MatchablePart] -> MatcherState a
unmatchableAsError s ps = customError $ "\"" ++ show ps ++ "\" couldn't be understood as a " ++ s

--


-- Auxiliary

-- Adds the parameters in a title to the variables environment
registerTitleParameters :: Title -> MatcherState ()
registerTitleParameters [] = return ()
registerTitleParameters (TitleWords _:ts) = registerTitleParameters ts
registerTitleParameters (TitleParam n t : ts) = do
    r <- varIsDefined n
    when r $ alreadyDefinedParameterError n
    setVarType n t
    registerTitleParameters ts

-- Validates a sentence and persists its changes to the state, assuming that its arguments are correctly typed
commitSentence :: Title -> Sentence -> MatcherState ()
commitSentence fT (VarDef ns v) = do
    t <- getValueType v
    mapM_ (`setVarType` t) ns
commitSentence fT (If v ss) = commitSentences fT ss
commitSentence fT (IfElse v ssTrue ssFalse) = commitSentences fT $ ssTrue ++ ssFalse
commitSentence fT (ForEach n v ss) = do
    isDef <- varIsDefined n
    when isDef $ alreadyDefinedIteratorError n
    ~(ListT eT) <- getValueType v
    setVarType n eT
    commitSentences fT ss
    removeVarType n
commitSentence fT (Until v ss) = commitSentences fT ss
commitSentence fT (While v ss) = commitSentences fT ss
commitSentence fT (Result v) = return ()
commitSentence fT (ProcedureCall t vs) = return ()

-- Commits the sentences in a list in order
commitSentences :: Title -> [Sentence] -> MatcherState ()
commitSentences fT = mapM_ $ commitSentence fT

getValueType :: Value -> MatcherState Type
getValueType (IntV _) = return IntT
getValueType (FloatV _) = return FloatT
getValueType (BoolV _) = return BoolT
getValueType (ListV t _) = return $ ListT t
getValueType (VarV n) = fromJust <$> getVarType n
getValueType (OperatorCall t vs) = do
    vTs <- mapM getValueType vs
    opT <- getOperatorCallType t vTs
    return $ fromJust opT

-- Returns whether a type satisfies another type
satisfiesType :: Type -> Type -> MatcherState Bool
satisfiesType _ AnyT = return True
satisfiesType IntT FloatT = return True
satisfiesType FloatT IntT = return True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType t1 t2 = return $ t1 == t2

-- Returns whether two words match
isWord :: String -> String -> Bool
isWord w1 w2 = map toLower w1 == map toLower w2

--


-- Matchers

matchAsName :: [MatchablePart] -> MatcherState (Maybe Name)
matchAsName [WordP w] = return $ Just [w]
matchAsName (WordP w : ps) = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ Just (w:ws)
        Nothing -> return Nothing
matchAsName ps = return Nothing

matchAsInt :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsInt [IntP n] = return $ Just (IntV n)
matchAsInt ps = return Nothing

matchAsFloat :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsFloat [FloatP n] = return $ Just (FloatV n)
matchAsFloat ps = return Nothing

matchAsBool :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsBool [WordP s]
    | s `isWord` "true" = return $ Just (BoolV True)
    | s `isWord` "false" = return $ Just (BoolV False)
matchAsBool ps = return Nothing

matchAsVar :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsVar ps = do
    r <- matchAsName ps
    case r of
        Just n@(w:ws) -> do
            r <- matchAsVar' n
            case r of
                Just _ -> return r
                Nothing -> if w `isWord` "the" then matchAsVar' ws else return Nothing
        Nothing -> return Nothing
    where
        matchAsVar' :: Name -> MatcherState (Maybe Value)
        matchAsVar' n = do
            isDef <- varIsDefined n
            return $ if isDef then Just (VarV n) else Nothing

matchAsOperatorCall :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsOperatorCall _ = undefined

matchAsProcedureCall :: [MatchablePart] -> MatcherState (Maybe Sentence)
matchAsProcedureCall ps = undefined

matchAsValue :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = matchFirst ps [matchAsInt, matchAsFloat, matchAsBool, matchAsVar, matchAsOperatorCall]
    where
        matchFirst :: [MatchablePart] -> [[MatchablePart] -> MatcherState (Maybe Value)] -> MatcherState (Maybe Value)
        matchFirst _ [] = return Nothing
        matchFirst ps (f:fs) = do
            r <- f ps
            case r of
                Just _ -> return r
                Nothing -> matchFirst ps fs

matchValue :: Value -> MatcherState Value
matchValue (ListV t es) = do
    es' <- mapM matchValue es
    return $ ListV t es'
matchValue v@(ValueM ps) = do
    r <- matchAsValue ps
    case r of
        Just v' -> return v'
        Nothing -> unmatchableValueError v
matchValue v = return v

matchValueWithType :: Type -> Value -> MatcherState Value
matchValueWithType t v = do
    v' <- matchValue v
    t' <- getValueType v'
    r <- t `satisfiesType` t'
    unless r $ wrongTypeValueError v' t
    return v'

matchSentence :: Title -> Sentence -> MatcherState Sentence
matchSentence _ s@(SentenceM ps) = do
    r <- matchAsProcedureCall ps
    case r of
        Just s' -> return s'
        Nothing -> unmatchableSentenceError s
matchSentence _ (VarDef ns v) = do
    v' <- matchValue v
    return $ VarDef ns v'
matchSentence fT (If v ss) = do
    v' <- matchValueWithType BoolT v
    ss' <- matchSentences fT ss
    return $ If v' ss'
matchSentence fT (IfElse v ssTrue ssFalse) = do
    v' <- matchValueWithType BoolT v
    ssTrue' <- matchSentences fT ssTrue
    ssFalse' <- matchSentences fT ssFalse
    return $ IfElse v' ssTrue' ssFalse'
matchSentence fT (ForEach n v ss) = do
    v' <- matchValueWithType (ListT AnyT) v
    ss' <- matchSentences fT ss
    return $ ForEach n v' ss'
matchSentence fT (Until v ss) = do
    v' <- matchValueWithType BoolT v
    ss' <- matchSentences fT ss
    return $ Until v' ss
matchSentence fT (While v ss) = do
    v' <- matchValueWithType BoolT v
    ss' <- matchSentences fT ss
    return $ While v' ss
matchSentence fT (Result v) = do
    r <- getUserDefinedOperatorType fT
    case r of
        Just t -> do
            v' <- matchValueWithType t v
            return $ Result v'
        Nothing -> procedureReturnError fT
matchSentence _ s = return s

matchSentences :: Title -> [Sentence] -> MatcherState [Sentence]
matchSentences fT [] = return []
matchSentences fT (s:rest) = do
    s' <- matchSentence fT s
    commitSentence fT s'
    rest' <- matchSentences fT rest
    return $ s':rest'

-- Matches the matchables in each sentence of a block
matchBlock :: Block -> MatcherState Block
matchBlock (FunDef t ss) = do
    registerTitleParameters t
    ss' <- matchSentences t ss
    return $ FunDef t ss'

-- Matches the matchables in each block of a program
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
            --p1 <- registerStructs p
            --p2 <- registerFunctions p1
            matchBlocks p

--
