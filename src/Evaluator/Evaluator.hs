module Evaluator ( module Evaluator, ReadWrite(..) ) where

import Data.List ( find )
import Data.Maybe ( fromJust, mapMaybe )
import Control.Monad ( void )

import EvaluatorEnv
import BuiltInDefs
import BuiltInEval
import SolverEnv ( SolverData, getLocation )
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
variablesFromTitle :: ReadWrite m => [Bare TitlePart] -> [Bare Value] -> EvaluatorEnv m ([(Name, Bare Value)], [(Name, Int)])
variablesFromTitle _ [] = return ([], [])
variablesFromTitle (TitleWords _ _ : ts) vs = variablesFromTitle ts vs
variablesFromTitle (TitleParam _ pn (RefT _) : ts) ((RefV _ addr):vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    return (vars, (pn, addr):refs)
variablesFromTitle (TitleParam _ pn _ : ts) (v:vs) = do
    (vars, refs) <- variablesFromTitle ts vs
    return ((pn, v):vars, refs)
variablesFromTitle [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

-- Finishes evaluating the arguments of a function call if necessary
evaluateParameters :: ReadWrite m => [Bare TitlePart] -> [Annotated Value] -> EvaluatorEnv m [Bare Value]
evaluateParameters ts vs = do
    l <- getCurrentLocation
    r <- evaluateParameters' ts vs
    setCurrentLocation l
    return r
    where
        evaluateParameters' :: ReadWrite m => [Bare TitlePart] -> [Annotated Value] -> EvaluatorEnv m [Bare Value]
        evaluateParameters' _ [] = return []
        evaluateParameters' (TitleWords _ _ : ts) vs = evaluateParameters' ts vs
        evaluateParameters' (TitleParam _ _ (RefT _) : ts) (v:vs) = do
            v' <- withLocation v evaluateValueExceptReference
            (v':) <$> evaluateParameters' ts vs
        evaluateParameters' (TitleParam {} : ts) (v:vs) = do
            v' <- withLocation v evaluateValue
            (v':) <$> evaluateParameters' ts vs
        evaluateParameters' [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

hasIterators :: Value a -> Bool
hasIterators (IterV {}) = True
hasIterators (OperatorCall _ _ vs) = any hasIterators vs
hasIterators _ = False

--


-- Evaluators

evaluateValueExceptReference :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateValueExceptReference (VarV _ vn) = do
    r <- getVariableAddress vn
    case r of
        Just addr -> return $ RefV () addr
        Nothing -> throwHere $ UndefinedVariable vn
evaluateValueExceptReference (RefV _ addr) = return $ RefV () addr
evaluateValueExceptReference v = evaluateValue v

evaluateValue :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateValue (VarV _ vn) = do
    r <- getVariableValue vn
    case r of
        Just v -> return v
        Nothing -> throwHere $ UndefinedVariable vn
evaluateValue (RefV _ addr) = getValueAtAddress addr
evaluateValue (ListV _ t es) = do
    l' <- ListV () t <$> mapM (`withLocation` evaluateValue) es
    saveReferences l'
evaluateValue (OperatorCall _ fid vs) = evaluateOperator fid vs
evaluateValue v@(IntV _ _) = return $ void v
evaluateValue v@(FloatV _ _) = return $ void v
evaluateValue v@(BoolV _ _) = return $ void v
evaluateValue v@(CharV _ _) = return $ void v
evaluateValue (IterV {}) = error "Shouldn't happen: values with iterators must be solved before evaluating them"
evaluateValue (ValueM _ _) = error "Shouldn't happen: values must be solved before evaluating them"

evaluateSentences :: ReadWrite m => [Annotated Sentence] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateSentences [] = return Nothing
evaluateSentences ss = do
    firstNotNull evaluateSentenceWithLocation ss
    where
        evaluateSentenceWithLocation :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
        evaluateSentenceWithLocation s = do
            let ann = getLocation s
            r <- withLocation s evaluateSentence
            setCurrentLocation ann
            return r

evaluateSentence :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
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
    let iterateLoop = (\(RefV _ ref) -> setVariableAddress iN ref >> evaluateSentences ls)
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
evaluateSentence (SentenceM _ _) = error "Shouldn't happen: sentences must be solved before evaluating them"

evaluateOperator :: ReadWrite m => FunId -> [Annotated Value] -> EvaluatorEnv m (Bare Value)
evaluateOperator fid vs
    | isBuiltInFunction fid = do
        ~(FunCallable (Title _ t) _) <- fromJust <$> getFunctionCallable fid
        vs' <- evaluateParameters t vs
        evaluateBuiltInOperator fid vs'
    | otherwise = do
        r <- evaluateUserDefinedFunction fid vs
        case r of
            Just r' -> return r'
            Nothing -> throwHere ExpectedResult

evaluateProcedure :: ReadWrite m => FunId -> [Annotated Value] -> EvaluatorEnv m ()
evaluateProcedure fid vs
    | isBuiltInFunction fid = do
        ~(FunCallable (Title _ t) _) <- fromJust <$> getFunctionCallable fid
        vs' <- evaluateParameters t vs
        evaluateBuiltInProcedure fid vs'
    | otherwise = void $ evaluateUserDefinedFunction fid vs

evaluateUserDefinedFunction :: ReadWrite m => FunId -> [Annotated Value] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateUserDefinedFunction fid vs = do
    ~(FunCallable (Title _ t) ss) <- fromJust <$> getFunctionCallable fid
    vs' <- evaluateParameters t vs
    (vars, refs) <- variablesFromTitle t vs'
    withVariables (evaluateSentences ss) vars refs

--


-- Main

evaluateProgram :: ReadWrite m => Program -> SolverData -> m (Either Error (((), Location), EvaluatorData))
evaluateProgram prog s = runEvaluatorEnv (evaluateProgram' prog s) initialState initialLocation
    where
        evaluateProgram' :: ReadWrite m => Program -> SolverData -> EvaluatorEnv m ()
        evaluateProgram' prog (fs, _) = do
            setFunctions $ translateFunctions prog fs
            evaluateProcedure "run" []

--
