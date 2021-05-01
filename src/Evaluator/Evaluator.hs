module Evaluator where

import Data.List ( find )
import Data.Maybe ( fromJust, mapMaybe )
import Control.Monad ( void )

import EvaluatorEnv
import BuiltInDefs
import BuiltInEval
import ParserEnv ( ParserData )
import Utils ( firstNotNull )
import Errors
import AST

--


-- Auxiliary

translateFunction :: Program -> (FunId, FunSignature) -> Maybe (FunId, FunCallable)
translateFunction p (fid, FunSignature t _) =
    case findDefinition p t of
        (Just (FunDef _ _ _ ss)) -> Just (fid, FunCallable t ss)
        Nothing -> Nothing
    where
        findDefinition :: Program -> Title a -> Maybe (Annotated Block)
        findDefinition p t = find (\(FunDef _ t' _ _) -> void t == void t') p

translateState :: Program -> ParserData -> EvaluatorEnv ()
translateState prog (fs, _) = setFunctions $ mapMaybe (translateFunction prog) fs

variablesFromTitle :: [Bare TitlePart] -> [Bare Value] -> EvaluatorEnv ([(Name, Bare Value)], [(Name, Int)])
variablesFromTitle _ [] = return ([], [])
variablesFromTitle (TitleWords _ _ : ts) vs = variablesFromTitle ts vs
variablesFromTitle (TitleParam _ pn RefT : ts) ((VarV _ vn):vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    ~(Just addr) <- getVariableAddress vn
    return (vars, (pn, addr):refs)
variablesFromTitle (TitleParam _ pn _ : ts) (v:vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    return ((pn, v):vars, refs)

--


-- Evaluators

evaluateValue :: Value a -> EvaluatorEnv (Bare Value)
evaluateValue (VarV _ vn) = do
    r <- getVariableValue vn
    case r of
        Just v -> return v
        Nothing -> throw $ undefinedVariableError vn
evaluateValue (ListV _ t es) = do
    es' <- mapM evaluateValue es
    return $ ListV () t es'
evaluateValue (OperatorCall _ fid vs) = do
    vs' <- mapM evaluateValue vs
    evaluateOperator fid vs'
evaluateValue v = return $ void v

evaluateSentenceLines :: [Annotated Sentence] -> EvaluatorEnv (Maybe (Bare Value))
evaluateSentenceLines [] = return Nothing
evaluateSentenceLines ss = firstNotNull (\s -> setCurrentLocation (getAnnotation s) >> evaluateSentence s) ss

evaluateSentence :: Annotated Sentence -> EvaluatorEnv (Maybe (Bare Value))
evaluateSentence (VarDef _ vNs v) = do
    v' <- evaluateValue v
    mapM_ (`setVariableValue` v') vNs
    return Nothing
evaluateSentence (If _ bv ls) = do
    (BoolV _ v') <- evaluateValue bv
    if v'
        then evaluateSentenceLines ls
        else return Nothing
evaluateSentence (IfElse _ v lsT lsF) = do
    (BoolV _ v') <- evaluateValue v
    if v'
        then evaluateSentenceLines lsT
        else evaluateSentenceLines lsF
evaluateSentence (ForEach _ iN lv ls) = do
    (ListV _ _ v') <- evaluateValue lv
    let iterateLoop = (\v -> setVariableValue iN v >> evaluateSentenceLines ls)
    r <- firstNotNull iterateLoop v'
    removeVariableValue iN
    return r
evaluateSentence s@(Until _ bv ls) = do
    (BoolV _ v') <- evaluateValue bv
    if v'
        then return Nothing
        else  do
            r <- evaluateSentenceLines ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
evaluateSentence s@(While _ bv ls) = do
    (BoolV _ v') <- evaluateValue bv
    if v'
        then do
            r <- evaluateSentenceLines ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
        else return Nothing
evaluateSentence (Result _ v) = do
    v' <- evaluateValue v
    return $ Just v'
evaluateSentence (ProcedureCall _ fid vs) = do
    vs' <- mapM evaluateValue vs
    evaluateProcedure fid vs'
    return Nothing

evaluateOperator :: FunId -> [Value a] -> EvaluatorEnv (Bare Value)
evaluateOperator fid vs
    | isBuiltInFunction fid = evaluateBuiltInOperator fid vs
    | otherwise = do
        r <- evaluateUserDefinedFunction fid vs
        maybe (throw expectedResultError) return r

evaluateProcedure :: FunId -> [Value a] -> EvaluatorEnv ()
evaluateProcedure fid vs
    | isBuiltInFunction fid = evaluateBuiltInProcedure fid vs
    | otherwise = void $ evaluateUserDefinedFunction fid vs

evaluateUserDefinedFunction :: FunId -> [Value a] -> EvaluatorEnv (Maybe (Bare Value))
evaluateUserDefinedFunction fid vs = do
    r <- getFunctionCallable fid
    case r of
        Just (FunCallable (Title _ t) ss) -> do
            (vars, refs) <- variablesFromTitle t (map void vs)
            withVariables (evaluateSentenceLines ss) vars refs
        Nothing -> throw $ functionNotDefinedError fid

evaluateProgram :: Program -> ParserData -> IO EvaluatorData
evaluateProgram prog s = do
    r <- runEvaluatorEnv (evaluateProgram' prog s) initialState initialLocation
    case r of
        Left e -> error e
        Right r -> return $ snd r
    where
        evaluateProgram' :: Program -> ParserData -> EvaluatorEnv ()
        evaluateProgram' prog s = do
            translateState prog s
            evaluateProcedure "run" []

--
