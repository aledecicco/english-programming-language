module Evaluator ( module Evaluator, ReadWrite(..) ) where

import Data.List ( find )
import Control.Monad ( void, unless )

import EvaluatorEnv
import BuiltInDefs
import BuiltInEval
import SolverEnv ( SolverData )
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
                -- If a function is not found in the written program, then it must be a built-in function
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
            v' <- withLocation v evaluateUpToReference
            (v':) <$> evaluateParameters' ts vs
        evaluateParameters' (TitleParam {} : ts) (v:vs) = do
            v' <- withLocation v evaluateValue
            (v':) <$> evaluateParameters' ts vs
        evaluateParameters' [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

hasIterators :: Value a -> Bool
hasIterators (IterV {}) = True
hasIterators (OperatorCall _ _ vs) = any hasIterators vs
hasIterators _ = False

copyValue :: ReadWrite m => Bare Value -> EvaluatorEnv m (Bare Value)
copyValue (ListV _ eT es) = ListV () eT <$> mapM copyValue es
copyValue (RefV _ addr) = do
    v <- getValueAtAddress addr
    v' <- copyValue v
    RefV () <$> addValue v'
copyValue v = return v

--


-- Evaluators

evaluateReferences :: ReadWrite m => Bare Value -> EvaluatorEnv m (Bare Value)
evaluateReferences (VarV _ vn) = do
    isDef <- variableIsDefined vn
    unless isDef $ throwHere (UndefinedVariable vn)
    getVariableValue vn >>= evaluateReferences
evaluateReferences (RefV _ addr) = getValueAtAddress addr >>= evaluateReferences
evaluateReferences l@(ListV {}) = copyValue l
evaluateReferences v = return v

evaluateUpToReference :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateUpToReference (VarV _ vn) = do
    isDef <- variableIsDefined vn
    unless isDef $ throwHere (UndefinedVariable vn)
    RefV () <$> getVariableAddress vn
evaluateUpToReference (OperatorCall _ fid vs) = evaluateOperator fid vs
evaluateUpToReference (ListV _ eT es) = do
    es' <- mapM (`withLocation` evaluateValue) es
    addrs <- mapM addValue es'
    return $ ListV () eT (map (RefV ()) addrs)
evaluateUpToReference v = return $ void v

evaluateValue :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateValue (IterV {}) = error "Shouldn't happen: values with iterators must be solved before evaluating them"
evaluateValue (ValueM _ _) = error "Shouldn't happen: values must be solved before evaluating them"
evaluateValue v = evaluateUpToReference v >>= evaluateReferences

evaluateSentences :: ReadWrite m => [Annotated Sentence] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateSentences [] = return Nothing
evaluateSentences ss = inContainedScope $ firstNotNull evaluateSentenceWithLocation ss
    where
        evaluateSentenceWithLocation :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
        evaluateSentenceWithLocation s = do
            let ann = getLocation s
            r <- withLocation s evaluateSentence
            setCurrentLocation ann
            return r

evaluateSentence :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
evaluateSentence s = tick >> evaluateSentence' s
    where
        evaluateSentence' :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
        evaluateSentence' (VarDef _ ~(vn:vns) _ v) = do
            v' <- withLocation v evaluateValue
            setVariableValue vn v'
            mapM_ (\vn' -> copyValue v' >>= setVariableValue vn') vns
            return Nothing
        evaluateSentence' (If _ bv ls) = do
            ~(BoolV _ v') <- withLocation bv evaluateValue
            if v'
                then evaluateSentences ls
                else return Nothing
        evaluateSentence' (IfElse _ bv lsT lsF) = do
            ~(BoolV _ v') <- withLocation bv evaluateValue
            if v'
                then evaluateSentences lsT
                else evaluateSentences lsF
        evaluateSentence' (ForEach _ iN _ lv ls) = do
            let auxName = "_" : iN
            lv' <- withLocation lv evaluateUpToReference
            es <- case lv' of
                (ListV _ _ es) -> do
                    addr <- addValue lv'
                    setVariableAddress auxName addr
                    return es
                (RefV _ addr) -> do
                    ~(ListV _ _ es) <- getValueAtAddress addr
                    setVariableAddress auxName addr
                    return es
                _ -> error "Shouldn't happen: wrong type in loop"
            let iterateLoop = (\(RefV _ ref) -> setVariableAddress iN ref >> evaluateSentences ls)
            r <- firstNotNull iterateLoop es
            removeVariable iN
            removeVariable auxName
            return r
        evaluateSentence' s@(Until _ bv ls) = do
            ~(BoolV _ v') <- withLocation bv evaluateValue
            if v'
                then return Nothing
                else do
                    r <- evaluateSentences ls
                    case r of
                        (Just v'') -> return $ Just v''
                        Nothing -> evaluateSentence s
        evaluateSentence' s@(While _ bv ls) = do
            ~(BoolV _ v') <- withLocation bv evaluateValue
            if v'
                then do
                    r <- evaluateSentences ls
                    case r of
                        (Just v'') -> return $ Just v''
                        Nothing -> evaluateSentence s
                else return Nothing
        evaluateSentence' (Result _ v) = do
            v' <- withLocation v evaluateValue
            return $ Just v'
        evaluateSentence' (ProcedureCall _ fid vs) = evaluateProcedure fid vs >> return Nothing
        evaluateSentence' (SentenceM _ _) = error "Shouldn't happen: sentences must be solved before evaluating them"

evaluateOperator :: ReadWrite m => FunId -> [Annotated Value] -> EvaluatorEnv m (Bare Value)
evaluateOperator fid vs
    | isBuiltInFunction fid = do
        (FunCallable (Title _ t) _) <- getFunctionCallable fid
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
        (FunCallable (Title _ t) _) <- getFunctionCallable fid
        vs' <- evaluateParameters t vs
        evaluateBuiltInProcedure fid vs'
    | otherwise = void $ evaluateUserDefinedFunction fid vs

evaluateUserDefinedFunction :: ReadWrite m => FunId -> [Annotated Value] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateUserDefinedFunction fid vs = do
    (FunCallable (Title _ t) ss) <- getFunctionCallable fid
    vs' <- evaluateParameters t vs
    (vars, refs) <- variablesFromTitle t vs'
    inNewScope (evaluateSentences ss) vars refs

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
