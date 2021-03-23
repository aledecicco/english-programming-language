module Evaluator where

import Data.List ( find )
import Data.Maybe ( fromJust )
import Control.Monad ( void )
import Control.Applicative ( (<|>) )

import EvaluatorEnv
import ParserEnv ( ParserState )
import Utils ( firstNotNull )
import Errors
import AST

--


-- Auxiliary

translateState :: Program -> ParserState -> EvaluatorState
translateState p (fE, _, _) =
    let funSentences = map (translateFunction p) fE
    in (funSentences, [], 0)
    where
        translateFunction :: Program -> (FunctionId, Function) -> (FunctionId, [SentenceLine])
        translateFunction p (fid, Function t _) =
            let (FunDef _ _ ss) = findDefinition p t
            in (fid, ss)
        findDefinition :: Program -> Title -> Block
        findDefinition p t = fromJust $ find (\(FunDef (Line _ t') _ _) -> t == t') p

--


-- Evaluators

evaluateValue :: Value -> EvaluatorEnv Value
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
        else evaluateSentenceLines ls <|> evaluateSentence s
evaluateSentence s@(While bv ls) = do
    (BoolV v') <- evaluateValue bv
    if v'
        then evaluateSentenceLines ls <|> evaluateSentence s
        else return Nothing
evaluateSentence (Result v) = do
    v' <- evaluateValue v
    return $ Just v'
evaluateSentence (ProcedureCall fid vs) = do
    vs' <- mapM evaluateValue vs
    evaluateProcedure fid vs'
    return Nothing

evaluateOperator :: FunctionId -> [Value] -> EvaluatorEnv Value
evaluateOperator fid vs = do
    ss <- fromJust <$> getFunctionSentences fid
    r <- evaluateSentenceLines ss
    case r of
        Just v -> return v
        Nothing -> expectedResultError

evaluateProcedure :: FunctionId -> [Value] -> EvaluatorEnv ()
evaluateProcedure fid vs = do
    ss <- fromJust <$> getFunctionSentences fid
    void $ evaluateSentenceLines ss

--


-- Main

runEvaluator :: EvaluatorEnv r -> EvaluatorState -> IO (Either Error (r, EvaluatorState))
runEvaluator = runEvaluatorEnv

evaluateProgram :: Program -> ParserState -> IO EvaluatorState
evaluateProgram p s = do
    r <- runEvaluator evaluateProgram' (translateState p s)
    case r of
        Left e -> error e
        Right r -> return $ snd r
    where
        evaluateProgram' :: EvaluatorEnv ()
        evaluateProgram' = evaluateProcedure "run" []

--
