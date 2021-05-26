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

translateFunctions :: Program -> [(FunId, FunSignature)] -> [(FunId, FunCallable)]
translateFunctions prog = map (translateFunction prog)
    where
        translateFunction :: Program -> (FunId, FunSignature) -> (FunId, FunCallable)
        translateFunction p (fid, FunSignature t _) =
            case findDefinition p t of
                (Just (FunDef _ _ _ ss)) -> (fid, FunCallable t ss)
                -- if a function is not found in the written program, then it must be a built-in function
                Nothing -> (fid, FunCallable t [])
        findDefinition :: Program -> Title a -> Maybe (Annotated Block)
        findDefinition p t = find (\(FunDef _ t' _ _) -> void t == void t') p

-- Returns a list of new variables to be declared and a list of references to be set according to the signature of a function
variablesFromTitle :: [Bare TitlePart] -> [Bare Value] -> EvaluatorEnv ([(Name, Bare Value)], [(Name, Int)])
variablesFromTitle _ [] = return ([], [])
variablesFromTitle (TitleWords _ _ : ts) vs = variablesFromTitle ts vs
variablesFromTitle (TitleParam _ pn (RefT _) : ts) ((VarV _ vn):vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    ~(Just addr) <- getVariableAddress vn
    return (vars, (pn, addr):refs)
variablesFromTitle (TitleParam _ pn _ : ts) (v:vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    return ((pn, v):vars, refs)

-- Finishes evaluating the arguments of a function call if necessary
evaluateParameters :: [Bare TitlePart] -> [Annotated Value] -> EvaluatorEnv [Bare Value]
evaluateParameters ts vs = do
    l <- getCurrentLocation
    r <- evaluateParameters' ts vs
    setCurrentLocation l
    return r
    where
        evaluateParameters' :: [Bare TitlePart] -> [Annotated Value] -> EvaluatorEnv [Bare Value]
        evaluateParameters' _ [] = return []
        evaluateParameters' (TitleWords _ _ : ts) vs = evaluateParameters' ts vs
        evaluateParameters' (TitleParam _ pn (RefT _) : ts) (v:vs) = do
            v' <- withLocation v evaluateValueExceptReference
            (v':) <$> evaluateParameters' ts vs
        evaluateParameters' (TitleParam _ pn _ : ts) (v:vs) = do
            v' <- withLocation v evaluateValue
            (v':) <$> evaluateParameters' ts vs

--


-- Evaluators

evaluateValueExceptReference :: Annotated Value -> EvaluatorEnv (Bare Value)
evaluateValueExceptReference v@(VarV _ _) = return $ void v
evaluateValueExceptReference v = evaluateValue v

evaluateValue :: Annotated Value -> EvaluatorEnv (Bare Value)
evaluateValue (VarV _ vn) = do
    r <- getVariableValue vn
    case r of
        Just v -> return v
        Nothing -> throwHere $ UndefinedVariable vn
evaluateValue (ListV _ t es) = do
    es' <- mapM (`withLocation` evaluateValue) es
    return $ ListV () t es'
evaluateValue (OperatorCall _ fid vs) = evaluateOperator fid vs
evaluateValue v = return $ void v

evaluateSentences :: [Annotated Sentence] -> EvaluatorEnv (Maybe (Bare Value))
evaluateSentences [] = return Nothing
evaluateSentences ss = firstNotNull (`withLocation` evaluateSentence) ss

evaluateSentence :: Annotated Sentence -> EvaluatorEnv (Maybe (Bare Value))
evaluateSentence (VarDef _ vNs v) = do
    v' <- withLocation v evaluateValue
    mapM_ (`setVariableValue` v') vNs
    return Nothing
evaluateSentence (If _ bv ls) = do
    ~(BoolV _ v') <- withLocation bv evaluateValue
    if v'
        then evaluateSentences ls
        else return Nothing
evaluateSentence (IfElse _ bv lsT lsF) = do
    ~(BoolV _ v') <- withLocation bv evaluateValue
    if v'
        then evaluateSentences lsT
        else evaluateSentences lsF
evaluateSentence (ForEach _ iN lv ls) = do
    ~(ListV _ _ v') <- withLocation lv evaluateValue
    let iterateLoop = (\v -> setVariableValue iN v >> evaluateSentences ls)
    r <- firstNotNull iterateLoop v'
    removeVariableValue iN
    return r
evaluateSentence s@(Until _ bv ls) = do
    ~(BoolV _ v') <- withLocation bv evaluateValue
    if v'
        then return Nothing
        else  do
            r <- evaluateSentences ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
evaluateSentence s@(While _ bv ls) = do
    ~(BoolV _ v') <- withLocation bv evaluateValue
    if v'
        then do
            r <- evaluateSentences ls
            case r of
                (Just v'') -> return $ Just v''
                Nothing -> evaluateSentence s
        else return Nothing
evaluateSentence (Result _ v) = do
    v' <- withLocation v evaluateValue
    return $ Just v'
evaluateSentence (ProcedureCall _ fid vs) = evaluateProcedure fid vs >> return Nothing

evaluateOperator :: FunId -> [Annotated Value] -> EvaluatorEnv (Bare Value)
evaluateOperator fid vs
    | isBuiltInFunction fid = do
        ~(FunCallable (Title _ t) _) <- fromJust <$> getFunctionCallable fid
        vs' <- evaluateParameters t vs
        evaluateBuiltInOperator fid vs'
    | otherwise = do
        r <- evaluateUserDefinedFunction fid vs
        maybe (throwHere ExpectedResult) return r

evaluateProcedure :: FunId -> [Annotated Value] -> EvaluatorEnv ()
evaluateProcedure fid vs
    | isBuiltInFunction fid = do
        ~(FunCallable (Title _ t) _) <- fromJust <$> getFunctionCallable fid
        vs' <- evaluateParameters t vs
        evaluateBuiltInProcedure fid vs'
    | otherwise = void $ evaluateUserDefinedFunction fid vs

evaluateUserDefinedFunction :: FunId -> [Annotated Value] -> EvaluatorEnv (Maybe (Bare Value))
evaluateUserDefinedFunction fid vs = do
    r <- getFunctionCallable fid
    case r of
        Just (FunCallable (Title _ t) ss) -> do
            vs' <- evaluateParameters t vs
            (vars, refs) <- variablesFromTitle t vs'
            withVariables (evaluateSentences ss) vars refs
        Nothing -> throwHere $ UndefinedFunction fid

--


-- Main

evaluateProgram :: Program -> ParserData -> IO (Either Error (((), Location), EvaluatorData))
evaluateProgram prog s = runEvaluatorEnv (evaluateProgram' prog s) initialState initialLocation
    where
        evaluateProgram' :: Program -> ParserData -> EvaluatorEnv ()
        evaluateProgram' prog (fs, _) = do
            setFunctions $ translateFunctions prog fs
            evaluateProcedure "run" []

--
