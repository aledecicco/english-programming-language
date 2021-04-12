module Solver where

import Data.Maybe ( fromJust, isNothing )
import Data.List ( find )
import Control.Monad ( unless, when )

import Utils (getFunId)
import BuiltInDefs
import Matcher
import ParserEnv
import AST
import Errors

--


-- Types information

satisfiesType :: Type -> Type -> Bool
satisfiesType _ (AnyT _) = True
satisfiesType IntT FloatT = True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType t1 t2 = t1 == t2

getValueType :: Value -> ParserEnv Type
getValueType (IntV _) = return IntT
getValueType (FloatV _) = return FloatT
getValueType (BoolV _) = return BoolT
getValueType (CharV _) = return CharT
getValueType (ListV t _) = return $ ListT t
getValueType (VarV n) = fromJust <$> getVariableType n
getValueType (OperatorCall fid vs) = do
    vTs <- mapM getValueType vs
    getOperatorCallType fid vTs
    where
        getOperatorCallType :: FunId -> [Type] -> ParserEnv Type
        getOperatorCallType fid vTs = do
            ~(FunSignature _ (Operator tFun)) <- fromJust <$> getFunctionSignature fid
            return $ tFun vTs

--


-- Validations

setVariableTypeWithCheck :: Name -> Type -> ParserEnv ()
setVariableTypeWithCheck vn t = do
    r <- getVariableType vn
    case r of
        Just t' -> unless (t' `satisfiesType` t) $ mismatchingTypeAssignedError vn t t'
        Nothing -> setVariableType vn t

setNewVariableType :: Name -> Type -> ParserEnv ()
setNewVariableType vn t' = do
    isDef <- variableIsDefined vn
    if isDef
        then alreadyDefinedVariableError vn
        else setVariableType vn t'

checkValueType :: Value -> Type -> ParserEnv ()
checkValueType v t = do
    t' <- getValueType v
    unless (t' `satisfiesType` t) $ wrongTypeValueError t t'

-- Validates that a value is correctly formed
checkValueIntegrity :: Value -> ParserEnv ()
checkValueIntegrity (OperatorCall fid vs) = checkFunctionCallIntegrity (fid, vs)
checkValueIntegrity (ListV t vs) = mapM_ (\v -> checkValueIntegrity v >> checkValueType v t) vs
checkValueIntegrity _ = return ()

checkFunctionCallIntegrity :: (FunId, [Value]) -> ParserEnv ()
checkFunctionCallIntegrity (fid, vs) = do
    mapM_ checkValueIntegrity vs
    ~(FunSignature ft _) <- fromJust <$> getFunctionSignature fid
    checkParameterTypes [] ft vs
    where
        checkParameterTypes :: [(String, Type)] -> Title -> [Value] -> ParserEnv ()
        checkParameterTypes _ _ [] = return ()
        checkParameterTypes bts (TitleWords {} : ts) vs = checkParameterTypes bts ts vs
        checkParameterTypes bts (TitleParam n t : ts) (v:vs) = do
            t' <- getValueType v
            case findTypeToBind t of
                Just tid ->
                    case find (\bt -> fst bt == tid) bts of
                        Just (_, t) ->
                            if t' `satisfiesType` t
                                then checkParameterTypes bts ts vs
                                else wrongTypeParameterError t t' n
                        Nothing -> checkParameterTypes ((tid, t'):bts) ts vs
                Nothing ->
                    if t' `satisfiesType` t
                        then checkParameterTypes bts ts vs
                        else wrongTypeParameterError t t' n

        findTypeToBind :: Type -> Maybe String
        findTypeToBind (ListT t) = findTypeToBind t
        findTypeToBind (AnyT tid) = Just tid
        findTypeToBind _ = Nothing

--


-- Auxiliary

initialState :: ParserState
initialState = (builtInOperators ++ builtInProcedures, [], 0)

registerFunctions :: Program -> ParserEnv ()
registerFunctions = mapM_ registerFunction
    where
        registerFunction :: Block -> ParserEnv ()
        registerFunction (FunDef (Line _ ft) rt _) = do
            let fid = getFunId ft
            isDef <- functionIsDefined fid
            when isDef $ alreadyDefinedFunctionError fid
            let frt = case rt of
                    Just t -> Operator $ const t
                    Nothing -> Procedure
            setFunctionSignature fid $ FunSignature ft frt

registerParameters :: Title -> ParserEnv ()
registerParameters [] = return ()
registerParameters (TitleParam vn t : ts) = setNewVariableType vn t >> registerParameters ts
registerParameters (_ : ts) = registerParameters ts

--


-- Solvers

solveValue :: Value -> ParserEnv Value
solveValue (ListV t es) = do
    es' <- mapM (solveValueWithType t) es
    return $ ListV t es'
solveValue (ValueM ps) = do
    r <- matchAsValue ps
    case r of
        Just v' -> checkValueIntegrity v' >> return v'
        Nothing -> unmatchableValueError ps
solveValue v = return v

solveValueWithType :: Type -> Value -> ParserEnv Value
solveValueWithType t v = do
    v' <- solveValue v
    checkValueType v' t
    return v'

solveSentence :: Sentence -> Maybe Type -> ParserEnv Sentence
solveSentence (VarDef vNs v) _ = do
    v' <- solveValue v
    t <- getValueType v'
    mapM_ (\vn -> setVariableTypeWithCheck vn t) vNs
    return $ VarDef vNs v'
solveSentence (If v ls) rt = do
    v' <- solveValueWithType BoolT v
    ls' <- solveSentenceLines ls rt
    return $ If v' ls'
solveSentence (IfElse v lsT lsF) rt = do
    v' <- solveValueWithType BoolT v
    lsT' <- solveSentenceLines lsT rt
    lsF' <- solveSentenceLines lsF rt
    return $ IfElse v' lsT' lsF'
solveSentence (ForEach iN v ls) rt = do
    v' <- solveValueWithType (ListT $ AnyT "a") v
    ~(ListT t) <- getValueType v'
    setNewVariableType iN t
    ls' <- solveSentenceLines ls rt
    removeVariableType iN
    return $ ForEach iN v' ls'
solveSentence (Until v ls) rt = do
    v' <- solveValueWithType BoolT v
    ls' <- solveSentenceLines ls rt
    return $ Until v' ls'
solveSentence (While v ls) rt = do
    v' <- solveValueWithType BoolT v
    ls' <- solveSentenceLines ls rt
    return $ While v' ls'
solveSentence (Result v) rt =
    case rt of
        Just t -> do
            v' <- solveValueWithType t v
            return $ Result v'
        Nothing -> resultInProcedureError
solveSentence (SentenceM ps) _ = do
    r <- matchAsSentence ps
    case r of
        Just s@(ProcedureCall fid vs) -> checkFunctionCallIntegrity (fid, vs) >> return s
        _ -> unmatchableSentenceError ps

solveSentenceLine :: Line Sentence -> Maybe Type -> ParserEnv (Line Sentence)
solveSentenceLine (Line ln s) rt = do
    setLineNumber ln
    s' <- solveSentence s rt
    return $ Line ln s'

solveSentenceLines :: [Line Sentence] -> Maybe Type -> ParserEnv [Line Sentence]
solveSentenceLines ss rt = mapM (\s -> solveSentenceLine s rt) ss

solveBlock :: Block -> ParserEnv Block
solveBlock (FunDef tl@(Line ln t) rt ss) = do
    setLineNumber ln
    registerParameters t
    FunDef tl rt <$> solveSentenceLines ss rt

solveProgram :: Program -> (Program, ParserState)
solveProgram p =
    case runParserEnv (solveProgram' p) initialState of
        Left e -> error e
        Right r -> r
    where
        solveProgram' :: Program -> ParserEnv Program
        solveProgram' p = do
            registerFunctions p
            mapM (\b -> solveBlock b <* resetVariables) p

--
