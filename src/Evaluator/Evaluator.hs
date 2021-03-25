module Evaluator where

import Data.List ( find )
import Data.Maybe ( fromJust )
import Control.Monad ( void )
import Control.Applicative ( (<|>) )

import EvaluatorEnv
import PreludeEval
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
            case findDefinition p t of
                (Just (FunDef _ _ ss)) -> (fid, ss)
                Nothing -> (fid, [])
        findDefinition :: Program -> Title -> Maybe Block
        findDefinition p t = find (\(FunDef (Line _ t') _ _) -> t == t') p

isPreludeFunction :: FunctionId -> Bool
isPreludeFunction fid = fid `elem` ["print_%", "%_plus_%", "%_times_%", "the_first_element_of_%", "%_appended_to_%"]

--


-- Evaluators

evaluateValue :: Value -> EvaluatorEnv Value
evaluateValue (VarV vn) = fromJust <$> getVariableValue vn
evaluateValue (ListV t es) = do
    es' <- mapM evaluateValue es
    return $ ListV t es'
evaluateValue (OperatorCall fid vs) = do
    vs' <- mapM evaluateValue vs
    Evaluator.evaluateOperator fid vs'
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
    Evaluator.evaluateProcedure fid vs'
    return Nothing

evaluateOperator :: FunctionId -> [Value] -> EvaluatorEnv Value
evaluateOperator fid vs
    | isPreludeFunction fid = PreludeEval.evaluateOperator fid vs
    | otherwise = do
        ss <- fromJust <$> getFunctionSentences fid
        r <- evaluateSentenceLines ss
        case r of
            Just v -> return v
            Nothing -> expectedResultError

evaluateProcedure :: FunctionId -> [Value] -> EvaluatorEnv ()
evaluateProcedure fid vs
    | isPreludeFunction fid = PreludeEval.evaluateProcedure fid vs
    | otherwise = do
        ss <- fromJust <$> getFunctionSentences fid
        void $ evaluateSentenceLines ss

evaluateProgram :: Program -> ParserState -> IO EvaluatorState
evaluateProgram p s = do
    r <- runEvaluatorEnv evaluateProgram' (translateState p s)
    case r of
        Left e -> error e
        Right r -> return $ snd r
    where
        evaluateProgram' :: EvaluatorEnv ()
        evaluateProgram' = Evaluator.evaluateProcedure "run" []

--
