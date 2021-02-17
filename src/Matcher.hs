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
    removeVarType vn
    modify (\(fE, vE) -> (fE, (vn, t'):vE))

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

customError :: LineNumber -> String -> MatcherState a
customError ln e = lift . throwE $ "Error in line " ++ show ln ++ ":\n" ++ e ++ "\n"

lineText :: LineNumber -> String
lineText ln = "Error in line " ++ show ln ++ ":\n"

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
mismatchingTypeAssignError :: LineNumber -> Name -> Type -> Type -> MatcherState a
mismatchingTypeAssignError ln vn t t' = customError ln $
    "Can't assign value of type \"" ++ show t'
    ++ "\" to variable \"" ++ concat vn
    ++ "\" with type \"" ++ show t ++ "\""

-- Error that occurs when an undefined variable is used
undefinedVariableError :: LineNumber -> Name -> MatcherState a
undefinedVariableError ln vn = customError ln $
    "Variable \"" ++ concat vn ++ "\" is not defined"

-- Error that occurs when two parameters with the same name are used in a title
alreadyDefinedParameterError :: LineNumber -> Name -> MatcherState a
alreadyDefinedParameterError ln vn = customError ln $
    "Parameter name \"" ++ concat vn ++ "\" can't be used more than once"

-- Error that occurs when a loop uses an iterator with the same name as an already defined variable
alreadyDefinedIteratorError :: LineNumber -> Name -> MatcherState a
alreadyDefinedIteratorError ln vn = customError ln $
    "Variable \"" ++ concat vn ++ "\" is already in use"

-- Error that occurs when two functions with the same name are defined
alreadyDefinedFunctionError :: LineNumber -> Title -> MatcherState a
alreadyDefinedFunctionError ln t = customError ln $
    "Function title \"" ++ show t ++ "\" is already in use"

-- Error that occurs when a value of the wrong type is used in a sentence
wrongTypeValueError :: LineNumber -> Value -> Type -> MatcherState a
wrongTypeValueError ln v t = customError ln $
    "Expected value of type \"" ++ show t
    ++ "\", but got \"" ++ show v ++ "\" instead"

-- Error that occurs when a value of the wrong type is used as a parameter for a function
wrongTypeParameterError :: LineNumber -> Title -> Value -> Type -> Name -> MatcherState a
wrongTypeParameterError ln fT v t n = customError ln $
    "Function \"" ++ show fT ++ "\""
    ++ " expected value of type \"" ++ show t
    ++ "\" for parameter \"" ++ concat n
    ++ "\", but got \"" ++ show v ++ "\" instead"

-- Error that occurs when a value can't be understood
unmatchableValueError :: LineNumber -> Value -> MatcherState a
unmatchableValueError ln v = customError ln $
    "Couldn't understand the value \"" ++ show v ++ "\""

-- Error that occurs when a sentence can't be understood
unmatchableSentenceError :: LineNumber -> Sentence -> MatcherState a
unmatchableSentenceError ln s = customError ln $
    "Couldn't understand the sentence \"" ++ show s ++ "\""

-- Error that occurs when a return statement is used in a procedure
procedureReturnError :: LineNumber -> Title -> MatcherState a
procedureReturnError ln t = customError ln $
    "Function \"" ++ show t ++ "\" can't return a value"

--


-- Auxiliary

setVarTypeWithCheck :: LineNumber -> Name -> Type -> MatcherState ()
setVarTypeWithCheck ln vn t' = do
    r <- getVarType vn
    case r of
        Just t -> when (t /= t') $ mismatchingTypeAssignError ln vn t t'
        Nothing -> setVarType vn t'

getTitleParameters :: Title -> MatcherState [TitlePart]
getTitleParameters [] = return []
getTitleParameters (TitleWords _:ts) = getTitleParameters ts
getTitleParameters (p@TitleParam {} : ts) = (p:) <$> getTitleParameters ts

-- Adds the parameters in a title to the variables environment
registerTitleParameters :: TitleLine -> MatcherState ()
registerTitleParameters tl = do
    params <- getTitleParameters $ getLineContent tl
    mapM_ registerTitleParameter params
    where
        registerTitleParameter :: TitlePart -> MatcherState ()
        registerTitleParameter (TitleParam n t) = do
            let ln = getLineNumber tl
            r <- varIsDefined n
            when r $ alreadyDefinedParameterError ln n
            setVarTypeWithCheck ln n t

-- Validates a sentence and persists its changes to the state, assuming that its arguments are correctly typed
commitSentence :: TitleLine -> SentenceLine -> MatcherState ()
commitSentence tl (Line ln (VarDef ns v)) = do
    t <- getValueType v
    mapM_ (\n -> setVarTypeWithCheck ln n t) ns
commitSentence tl (Line ln (If v ss)) = commitSentences tl ss
commitSentence tl (Line ln (IfElse v ssTrue ssFalse)) = commitSentences tl $ ssTrue ++ ssFalse
commitSentence tl (Line ln (ForEach n v ss)) = do
    isDef <- varIsDefined n
    when isDef $ alreadyDefinedIteratorError ln n
    ~(ListT eT) <- getValueType v
    setVarTypeWithCheck ln n eT
    commitSentences tl ss
    removeVarType n
commitSentence tl (Line ln (Until v ss)) = commitSentences tl ss
commitSentence tl (Line ln (While v ss)) = commitSentences tl ss
commitSentence tl (Line ln (Result v)) = return ()
commitSentence tl (Line ln (ProcedureCall t vs)) = return ()

-- Commits the sentences in a list in order
commitSentences :: TitleLine -> [SentenceLine] -> MatcherState ()
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

-- Validates that a value is correctly formed
checkValueTypeIntegrity :: LineNumber -> Value -> MatcherState ()
checkValueTypeIntegrity ln (OperatorCall t vs) = checkFunctionCallIntegrity ln t vs
checkValueTypeIntegrity ln (ListV t vs) = mapM_ (checkValueTypeIntegrity ln) vs
checkValueTypeIntegrity _ _ = return ()

-- Validates that a function call is correctly formed
checkFunctionCallIntegrity :: LineNumber -> Title -> [Value] -> MatcherState ()
checkFunctionCallIntegrity ln fT vs = do
    mapM_ (checkValueTypeIntegrity ln) vs
    ps <-  getTitleParameters fT
    mapM_ checkParameterIntegrity $ zip vs ps
    where
        checkParameterIntegrity :: (Value, TitlePart) -> MatcherState ()
        checkParameterIntegrity (v, p@(TitleParam n t')) = do
            t <- getValueType v
            r <- t `satisfiesType` t'
            unless r $ wrongTypeParameterError ln fT v t' n

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

matchValue :: LineNumber -> Value -> MatcherState Value
matchValue ln (ListV t es) = do
    es' <- mapM (matchValue ln) es
    return $ ListV t es'
matchValue ln v@(ValueM ps) = do
    r <- matchAsValue ps
    case r of
        Just v' -> do
            checkValueTypeIntegrity ln v'
            return v'
        Nothing -> unmatchableValueError ln v
matchValue _ v = return v

matchValueWithType :: LineNumber -> Type -> Value -> MatcherState Value
matchValueWithType ln t v = do
    v' <- matchValue ln v
    t' <- getValueType v'
    r <- t `satisfiesType` t'
    unless r $ wrongTypeValueError ln v' t'
    return v'

matchSentence :: TitleLine -> SentenceLine -> MatcherState SentenceLine
matchSentence _ (Line ln s@(SentenceM ps)) = do
    r <- matchAsProcedureCall ps
    case r of
        Just s'@(ProcedureCall t vs) -> do
            checkFunctionCallIntegrity ln t vs
            return $ Line ln s'
        Nothing -> unmatchableSentenceError ln s
matchSentence _ (Line ln (VarDef ns v)) = do
    v' <- matchValue ln v
    return $ Line ln (VarDef ns v')
matchSentence tl (Line ln (If v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences tl ss
    return $ Line ln (If v' ss')
matchSentence tl (Line ln (IfElse v ssTrue ssFalse)) = do
    v' <- matchValueWithType ln BoolT v
    ssTrue' <- matchSentences tl ssTrue
    ssFalse' <- matchSentences tl ssFalse
    return $ Line ln (IfElse v' ssTrue' ssFalse')
matchSentence tl (Line ln (ForEach n v ss)) = do
    v' <- matchValueWithType ln (ListT AnyT) v
    ss' <- matchSentences tl ss
    return $ Line ln (ForEach n v' ss')
matchSentence tl (Line ln (Until v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences tl ss
    return $ Line ln (Until v' ss)
matchSentence tl (Line ln (While v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences tl ss
    return $ Line ln (While v' ss)
matchSentence tl (Line ln (Result v)) = do
    let fT = getLineContent tl
    r <- getUserDefinedOperatorType fT
    case r of
        Just t -> do
            v' <- matchValueWithType ln t v
            return $ Line ln (Result v')
        Nothing -> procedureReturnError ln fT
matchSentence _ s = return s

matchSentences :: TitleLine -> [SentenceLine] -> MatcherState [SentenceLine]
matchSentences tl [] = return []
matchSentences tl (s:rest) = do
    s' <- matchSentence tl s
    commitSentence tl s'
    rest' <- matchSentences tl rest
    return $ s':rest'

-- Matches the matchables in each sentence of a block
matchBlock :: Block -> MatcherState Block
matchBlock (FunDef tl ss) = do
    registerTitleParameters tl
    ss' <- matchSentences tl ss
    return $ FunDef tl ss'

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
            --registerFunctions p
            p' <- matchBlocks p
            return p'

--
