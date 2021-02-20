module Matcher where

import Control.Monad ( when, unless, void, filterM )
import Control.Monad.Trans.State ( get, gets, put, modify, liftCatch, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, catchE, runExcept, Except )
import Control.Monad.Trans.Class ( lift )
import Data.List ( find, intercalate )
import Data.Maybe ( isJust, isNothing, fromJust )
import Data.Bifunctor ( first, second )

import Types
import PreludeDefs
import Utils ( isWord, getLineContent )

--


-- Environment

type Error = String

type FunEnv = [(String, Function)]
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
    modify (second $ (:) (vn, t'))

removeVarType :: Name -> MatcherState ()
removeVarType vn = do
    vE <- getVarEnv
    let vE' = filter ((vn /=) . fst) vE
    setVarEnv vE'

setFunction :: Function -> MatcherState ()
setFunction f = do
    fid <- getFunctionId $ getTitle f
    modify (first $ (:) (fid, f))

getFunction :: FunctionId -> MatcherState (Maybe Function)
getFunction fid = do
    r <- find (\(fid', _) -> fid == fid') <$> getFunEnv
    case r of
        Just (_, f) -> return $ Just f
        Nothing -> return Nothing

getFunctionId :: Title -> MatcherState FunctionId
getFunctionId t = return $ intercalate "_" (getFunctionIdParts t)
    where
        getFunctionIdParts :: Title -> [FunctionId]
        getFunctionIdParts (TitleWords w : ts) = w ++ getFunctionIdParts ts
        getFunctionIdParts (TitleParam {} : ts) = "%" : getFunctionIdParts ts

functionIsDefined :: Title -> MatcherState Bool
functionIsDefined t = do
    fid <- getFunctionId t
    r <- getFunction fid
    case r of
        Just _ -> return True
        Nothing -> return False

isOperator :: FunctionId -> MatcherState Bool
isOperator fid = do
    r <- getFunction fid
    case r of
        Just Operator {} -> return True
        _ -> return False

isProcedure :: FunctionId -> MatcherState Bool
isProcedure fid = do
    r <- getFunction fid
    case r of
        Just Procedure {} -> return True
        _ -> return False

-- Returns the resulting type of a call to an operator
getOperatorCallType :: FunctionId -> [Type] -> MatcherState (Maybe Type)
getOperatorCallType fid vTs = do
    f <- getFunction fid
    case f of
        Just (Operator _ tFun) -> return $ Just (tFun vTs)
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

-- Error that occurs when a variable has a value of a given type and is assigned a value of a different type
mismatchingTypeAssignError :: LineNumber -> Name -> Type -> Type -> MatcherState a
mismatchingTypeAssignError ln vn t t' = customError ln $
    "Can't assign value of type \"" ++ show t'
    ++ "\" to variable \"" ++ concat vn
    ++ "\" with type \"" ++ show t ++ "\""

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

getTitle :: Function -> Title
getTitle (Operator t _) = t
getTitle (Procedure t) = t

getTitleParameters :: Title -> MatcherState [TitlePart]
getTitleParameters [] = return []
getTitleParameters (TitleWords _ : ts) = getTitleParameters ts
getTitleParameters (p@TitleParam {} : ts) = (p:) <$> getTitleParameters ts

-- Adds the parameters in a title to the variables environment
registerTitleParameters :: TitleLine -> MatcherState ()
registerTitleParameters (Line ln fT) = do
    params <- getTitleParameters fT
    mapM_ registerTitleParameter params
    where
        registerTitleParameter :: TitlePart -> MatcherState ()
        registerTitleParameter (TitleParam n t) = do
            r <- varIsDefined n
            when r $ alreadyDefinedParameterError ln n
            setVarTypeWithCheck ln n t

-- Validates a sentence and persists its changes to the state, assuming that its arguments are correctly typed
commitSentence :: SentenceLine -> MatcherState ()
commitSentence (Line ln (VarDef ns v)) = do
    t <- getValueType v
    mapM_ (\n -> setVarTypeWithCheck ln n t) ns
commitSentence (Line ln (If v ss)) = commitSentences ss
commitSentence (Line ln (IfElse v ssTrue ssFalse)) = commitSentences $ ssTrue ++ ssFalse
commitSentence (Line ln (ForEach n v ss)) = do
    isDef <- varIsDefined n
    when isDef $ alreadyDefinedIteratorError ln n
    ~(ListT eT) <- getValueType v
    setVarTypeWithCheck ln n eT
    commitSentences ss
    removeVarType n
commitSentence (Line ln (Until v ss)) = commitSentences ss
commitSentence (Line ln (While v ss)) = commitSentences ss
commitSentence (Line ln (Result v)) = return ()
commitSentence (Line ln (ProcedureCall t vs)) = return ()

-- Commits the sentences in a list in order
commitSentences :: [SentenceLine] -> MatcherState ()
commitSentences = mapM_ commitSentence

getValueType :: Value -> MatcherState Type
getValueType (IntV _) = return IntT
getValueType (FloatV _) = return FloatT
getValueType (BoolV _) = return BoolT
getValueType (ListV t _) = return $ ListT t
getValueType (VarV n) = fromJust <$> getVarType n
getValueType (OperatorCall fid vs) = do
    vTs <- mapM getValueType vs
    opT <- getOperatorCallType fid vTs
    return $ fromJust opT

-- Returns whether a type satisfies another type
satisfiesType :: Type -> Type -> MatcherState Bool
satisfiesType _ AnyT = return True
satisfiesType IntT FloatT = return True
satisfiesType FloatT IntT = return True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType t1 t2 = return $ t1 == t2

-- Validates that a value is correctly formed
checkValueTypeIntegrity :: LineNumber -> Value -> MatcherState ()
checkValueTypeIntegrity ln (OperatorCall fid vs) = do
    ~(Just (Operator t ps)) <- getFunction fid
    checkFunctionCallIntegrity ln t vs
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

-- Returns the result of the first action that returns a value when applied to the given element
firstNotNull :: a -> [a -> MatcherState (Maybe b)] -> MatcherState (Maybe b)
firstNotNull _ [] = return Nothing
firstNotNull e (f:fs) = do
    r <- f e
    case r of
        Just _ -> return r
        Nothing -> firstNotNull e fs

-- Returns all the ways to split a list into two, with at least one element in the first part
splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = splits' [x] xs
    where
        splits' :: [a] -> [a] -> [([a], [a])]
        splits' l [] = [(l,[])]
        splits' l (r:rs) = (l, r:rs) : splits' (l++[r]) rs

-- Returns whether a list of words a prefix of the given matchable, and the unmatched sufix
isPrefix :: [String] -> [MatchablePart] -> (Bool, [MatchablePart])
isPrefix [] ms = (True, ms)
isPrefix _ [] = (False, [])
isPrefix (t:ts) ms@(WordP w : ps) =
    if t == w
    then isPrefix ts ps
    else (False, ms)
isPrefix _ ms = (False, ms)

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
matchAsOperatorCall ps = do
    r <- matchAsFunctionCall ps
    case r of
        Just (fid, vs) -> do
            isOpCall <- isOperator fid
            return $ if isOpCall then Just (OperatorCall fid vs) else Nothing
        Nothing -> return Nothing

matchAsProcedureCall :: [MatchablePart] -> MatcherState (Maybe Sentence)
matchAsProcedureCall ps = do
    r <- matchAsFunctionCall ps
    case r of
        Just (fid, vs) -> do
            isProcCall <- isProcedure fid
            return $ if isProcCall then Just (ProcedureCall fid vs) else Nothing
        Nothing -> return Nothing

matchAsFunctionCall :: [MatchablePart] -> MatcherState (Maybe (FunctionId, [Value]))
matchAsFunctionCall ps = do
    fs <- map snd <$> getFunEnv
    firstNotNull ps $ map matchAsFunctionCall' fs
    where
        matchAsFunctionCall' :: Function -> [MatchablePart] -> MatcherState (Maybe (FunctionId, [Value]))
        matchAsFunctionCall' f ps = do
            let fT = getTitle f
            pos <- getTitleMatches fT ps
            return Nothing

getTitleMatches :: Title -> [MatchablePart] -> MatcherState [[[MatchablePart]]]
getTitleMatches (TitleWords ws : ts) ps = do
    let (isP, rest) = ws `isPrefix` ps
    if isP then getTitleMatches ts rest else return []
getTitleMatches (TitleParam _ _ : ts) ps = do
    let posSpans = splits ps
    lists <- mapM combineMatches posSpans
    return $ concat lists
    where
        combineMatches :: ([MatchablePart], [MatchablePart]) -> MatcherState [[[MatchablePart]]]
        combineMatches (span, rest) = do
            matches <- getTitleMatches ts rest
            return $ map (span:) matches

matchAsValue :: [MatchablePart] -> MatcherState (Maybe Value)
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = firstNotNull ps [matchAsInt, matchAsFloat, matchAsBool, matchAsVar, matchAsOperatorCall]

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

matchSentence :: Maybe Type -> Title -> SentenceLine -> MatcherState SentenceLine
matchSentence _ _ (Line ln s@(SentenceM ps)) = do
    r <- matchAsProcedureCall ps
    case r of
        Just s'@(ProcedureCall fid vs) -> do
            ~(Just (Procedure t)) <- getFunction fid
            checkFunctionCallIntegrity ln t vs
            return $ Line ln s'
        Nothing -> unmatchableSentenceError ln s
matchSentence _ _ (Line ln (VarDef ns v)) = do
    v' <- matchValue ln v
    return $ Line ln (VarDef ns v')
matchSentence rt fT (Line ln (If v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences rt fT ss
    return $ Line ln (If v' ss')
matchSentence rt fT (Line ln (IfElse v ssTrue ssFalse)) = do
    v' <- matchValueWithType ln BoolT v
    ssTrue' <- matchSentences rt fT ssTrue
    ssFalse' <- matchSentences rt fT ssFalse
    return $ Line ln (IfElse v' ssTrue' ssFalse')
matchSentence rt fT (Line ln (ForEach n v ss)) = do
    v' <- matchValueWithType ln (ListT AnyT) v
    ss' <- matchSentences rt fT ss
    return $ Line ln (ForEach n v' ss')
matchSentence rt fT (Line ln (Until v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences rt fT ss
    return $ Line ln (Until v' ss)
matchSentence rt fT (Line ln (While v ss)) = do
    v' <- matchValueWithType ln BoolT v
    ss' <- matchSentences rt fT ss
    return $ Line ln (While v' ss)
matchSentence rt fT (Line ln (Result v)) = do
    case rt of
        Just t -> do
            v' <- matchValueWithType ln t v
            return $ Line ln (Result v')
        Nothing -> procedureReturnError ln fT
matchSentence _ _ s = return s

matchSentences :: Maybe Type -> Title -> [SentenceLine] -> MatcherState [SentenceLine]
matchSentences rt fT [] = return []
matchSentences rt fT (s:rest) = do
    s' <- matchSentence rt fT s
    commitSentence s'
    rest' <- matchSentences rt fT rest
    return $ s':rest'

-- Matches the matchables in each sentence of a block
matchBlock :: Block -> MatcherState Block
matchBlock (FunDef tl rt ss) = do
    registerTitleParameters tl
    ss' <- matchSentences rt (getLineContent tl) ss
    return $ FunDef tl rt ss'

-- Matches the matchables in each block of a program
matchBlocks :: Program -> MatcherState Program
matchBlocks [] = return []
matchBlocks (b:bs) = do
    b' <- matchBlock b
    resetVariables
    bs' <- matchBlocks bs
    return $ b':bs'

--


-- Function registration

registerFunctions :: Program -> MatcherState ()
registerFunctions = mapM_ registerFunction
    where
        registerFunction :: Block -> MatcherState ()
        registerFunction (FunDef (Line ln fT) rt _) = do
            r <- functionIsDefined fT
            when r $ alreadyDefinedFunctionError ln fT
            setFunction $ case rt of
                Just t -> Operator fT (const t)
                Nothing -> Procedure fT

--


-- Main

-- Returns the given program with its matchables matched
matchProgram :: Program -> Either Error (Program, Env)
matchProgram p = runExcept $ runStateT matchProgram' initialEnv
    where
        matchProgram' :: MatcherState Program
        matchProgram' = registerFunctions p >> matchBlocks p

--
