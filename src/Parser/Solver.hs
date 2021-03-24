module Solver where

import Data.Maybe ( fromJust, isNothing )
import Control.Monad ( unless, when )

import Utils (getFunctionId)
import PreludeDefs
import Matcher
import ParserEnv
import AST
import Errors

--


-- Types information

satisfiesType :: Type -> Type -> Bool
satisfiesType _ AnyT = True
satisfiesType IntT FloatT = True
satisfiesType FloatT IntT = True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType t1 t2 = t1 == t2

getValueType :: Value -> ParserEnv Type
getValueType (IntV _) = return IntT
getValueType (FloatV _) = return FloatT
getValueType (BoolV _) = return BoolT
getValueType (ListV t _) = return $ ListT t
getValueType (VarV n) = fromJust <$> getVariableType n
getValueType (OperatorCall fid vs) = do
    vTs <- mapM getValueType vs
    getOperatorCallType fid vTs
    where
        getOperatorCallType :: FunctionId -> [Type] -> ParserEnv Type
        getOperatorCallType fid vTs = do
            ~(Function _ (Operator tFun)) <- fromJust <$> getFunction fid
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

-- Validates that a value is correctly formed
checkValueIntegrity :: Value -> ParserEnv ()
checkValueIntegrity (OperatorCall fid vs) = checkFunctionCallIntegrity (fid, vs)
checkValueIntegrity (ListV t vs) = mapM_ checkValueIntegrity vs
checkValueIntegrity _ = return ()

checkFunctionCallIntegrity :: (FunctionId, [Value]) -> ParserEnv ()
checkFunctionCallIntegrity (fid, vs) = do
    mapM_ checkValueIntegrity vs
    ~(Just (Function ft _)) <- getFunction fid
    checkParameterTypes ft vs
    where
        checkParameterTypes :: Title -> [Value] -> ParserEnv ()
        checkParameterTypes _ [] = return ()
        checkParameterTypes (TitleWords {} : ts) vs = checkParameterTypes ts vs
        checkParameterTypes (TitleParam n t : ts) (v:vs)  = do
            t' <- getValueType v
            if t' `satisfiesType` t
                then checkParameterTypes ts vs
                else wrongTypeParameterError v t n

--


-- Auxiliary

initialState :: ParserState
initialState = (operators ++ procedures, [], 0)

registerFunctions :: Program -> ParserEnv ()
registerFunctions = mapM_ registerFunction
    where
        registerFunction :: Block -> ParserEnv ()
        registerFunction (FunDef (Line _ ft) rt _) = do
            let fid = getFunctionId ft
            isDef <- functionIsDefined fid
            when isDef $ alreadyDefinedFunctionError ft
            let frt = case rt of
                    Just t -> Operator $ const t
                    Nothing -> Procedure
            setFunction fid $ Function ft frt

--


-- Solvers

solveValue :: Value -> ParserEnv Value
solveValue (ListV t es) = do
    es' <- mapM solveValue es
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
    t' <- getValueType v'
    if t' `satisfiesType` t
        then return v'
        else wrongTypeValueError v' t

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
    v' <- solveValueWithType (ListT AnyT) v
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
solveBlock (FunDef t rt ss) = FunDef t rt <$> solveSentenceLines ss rt

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
