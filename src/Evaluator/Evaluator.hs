module Evaluator where

import Data.List ( find )
import Data.Maybe ( fromJust, mapMaybe )
import Control.Monad ( void )

import EvaluatorEnv
import BuiltInDefs
import BuiltInEval
import ParserEnv ( ParserState )
import Utils ( firstNotNull )
import Errors
import AST

--


-- Auxiliary

translateFunction :: Program -> (FunId, FunSignature) -> Maybe (FunId, FunCallable)
translateFunction p (fid, FunSignature t _) =
    case findDefinition p t of
        (Just (FunDef _ _ ss)) -> Just (fid, FunCallable t ss)
        Nothing -> Nothing
    where
        findDefinition :: Program -> Title -> Maybe Block
        findDefinition p t = find (\(FunDef (Line _ t') _ _) -> t == t') p

translateState :: Program -> ParserState -> EvaluatorState
translateState p (fE, _, _) =
    let funSentences = mapMaybe (translateFunction p) fE
    in (funSentences, [], 0)

variablesFromTitle :: Title -> [Value] -> [(Name, Value)]
variablesFromTitle _ [] = []
variablesFromTitle (TitleWords _ : ts) vs = variablesFromTitle ts vs
variablesFromTitle (TitleParam n _ : ts) (v:vs) = (n, v) : variablesFromTitle ts vs

--


-- Evaluators

evaluateValue :: Value -> EvaluatorEnv Value
evaluateValue (VarV vn) = do
    r <- getVariableValue vn
    case r of
        Just v -> return v
        Nothing -> undefinedVariableError vn
evaluateValue (ListV t es) = do
    es' <- mapM evaluateValue es
    return $ ListV t es'
evaluateValue (OperatorCall fid vs) = do
    vs' <- mapM evaluateValue vs
    evaluateOperator fid vs'
evaluateValue v = return v

evaluateSentenceLines :: [SentenceLine] -> EvaluatorEnv (Maybe Value)
evaluateSentenceLines [] = return Nothing
evaluateSentenceLines ls = firstNotNull (\(Line ln s) -> setLineNumber ln >> evaluateSentence s) ls

evaluateSentence :: Sentence -> EvaluatorEnv (Maybe Value)
evaluateSentence (VarDef vNs v) = do
    v' <- evaluateValue v
    mapM_ (`setVariableValue` v') vNs
    return Nothing
evaluateSentence (If bv ls) = do
    (BoolV v') <- evaluateValue bv
    if v'
        then evaluateSentenceLines ls
        else return Nothing
evaluateSentence (IfElse v lsT lsF) = do
    (BoolV v') <- evaluateValue v
    if v'
        then evaluateSentenceLines lsT
        else evaluateSentenceLines lsF
evaluateSentence (ForEach iN lv ls) = do
    (ListV _ v') <- evaluateValue lv
    let iterateLoop = (\v -> setVariableValue iN v >> evaluateSentenceLines ls)
    r <- firstNotNull iterateLoop v'
    removeVariableValue iN
    return r
evaluateSentence s@(Until bv ls) = do
    (BoolV v') <- evaluateValue bv
    if v'
        then return Nothing
        else  do
            r <- evaluateSentenceLines ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
evaluateSentence s@(While bv ls) = do
    (BoolV v') <- evaluateValue bv
    if v'
        then do
            r <- evaluateSentenceLines ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
        else return Nothing
evaluateSentence (Result v) = do
    v' <- evaluateValue v
    return $ Just v'
evaluateSentence (ProcedureCall fid vs) = do
    vs' <- mapM evaluateValue vs
    evaluateProcedure fid vs'
    return Nothing

evaluateOperator :: FunId -> [Value] -> EvaluatorEnv Value
evaluateOperator fid vs
    | isBuiltInFunction fid = evaluateBuiltInOperator fid vs
    | otherwise = do
        r <- evaluateUserDefinedFunction fid vs
        maybe expectedResultError return r

evaluateProcedure :: FunId -> [Value] -> EvaluatorEnv ()
evaluateProcedure fid vs
    | isBuiltInFunction fid = evaluateBuiltInProcedure fid vs
    | otherwise = void $ evaluateUserDefinedFunction fid vs

evaluateUserDefinedFunction :: FunId -> [Value] -> EvaluatorEnv (Maybe Value)
evaluateUserDefinedFunction fid vs = do
    r <- getFunctionCallable fid
    case r of
        Just (FunCallable t ss) -> evaluateSentenceLines ss `withVariables` variablesFromTitle t vs
        Nothing -> functionNotDefinedError fid

evaluateProgram :: Program -> ParserState -> IO EvaluatorState
evaluateProgram p s = do
    r <- runEvaluatorEnv evaluateProgram' (translateState p s)
    case r of
        Left e -> error e
        Right r -> return $ snd r
    where
        evaluateProgram' :: EvaluatorEnv ()
        evaluateProgram' = evaluateProcedure "run" []

--
