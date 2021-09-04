{-|
Module      : Evaluator
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The language's evaluator.
-}

module Evaluator where

import Control.Monad (void, unless, (>=>))

import AST
import BuiltInDefs
import BuiltInEval
import Errors
import EvaluatorEnv
import Utils (firstNotNull, getFunId, hasIterators)


-- -----------------
-- * Auxiliary

-- | Registers the functions in a program so that they can be called from anywhere.
registerFunctions :: Monad m => Program -> EvaluatorEnv m ()
registerFunctions = mapM_ registerFunction
    where
        registerFunction :: Monad m => Annotated Block -> EvaluatorEnv m ()
        registerFunction (FunDef _ title@(Title _ parts) _ sentences) =
            let fid = getFunId parts
                callable = FunCallable (void title) sentences
            in setFunctionCallable fid callable

-- | Returns a list of new variables to be declared and a list of references to be set when calling a function according to its signature.
variablesFromTitle :: ReadWrite m =>
    [Bare TitlePart] -- ^ The title of the function being called.
    -> [Bare Value] -- ^ The values being passed as arguments.
    -> EvaluatorEnv m ([([Name], Bare Value)], [([Name], Int)]) -- ^ The names in the title that should point to new values and the ones that should point to existing addresses.
variablesFromTitle _ [] = return ([], [])
variablesFromTitle (TitleWords _ _ : rest) vals = variablesFromTitle rest vals
variablesFromTitle (TitleParam _ names (RefT _) : rest) ((RefV _ addr):vals) = do
    (vars, refs) <- variablesFromTitle rest vals
    return (vars, (names, addr):refs)
variablesFromTitle (TitleParam _ names _ : rest) (val:vals) = do
    (vars, refs) <- variablesFromTitle rest vals
    return ((names, val):vars, refs)
variablesFromTitle [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

-- | Finishes evaluating the references in the arguments of a function call if neccessary.
evaluateArguments :: ReadWrite m => [Bare TitlePart] -> [Bare Value] -> EvaluatorEnv m [Bare Value]
evaluateArguments _ [] = return []
evaluateArguments (TitleWords _ _ : rest) vals = evaluateArguments rest vals
evaluateArguments (TitleParam _ _ (RefT _) : rest) (val:vals) = (val:) <$> evaluateArguments rest vals
evaluateArguments (TitleParam {} : rest) (val:vals) = do
    val' <- evaluateReferences val
    (val':) <$> evaluateArguments rest vals
evaluateArguments [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

-- | Takes a list value and returns a list of the values it contains.
getListValues :: ReadWrite m => Bare Value -> EvaluatorEnv m [Bare Value]
getListValues (ListV _ _ vs) = return vs
getListValues (RefV _ addr) = getValueAtAddress addr >>= getListValues
getListValues _ = error "Shouldn't happen: value is not a list"

-- | Returns the list of values generated by a value with iterators.
getIteratorValues :: ReadWrite m => Annotated Value -> EvaluatorEnv m [Bare Value]
getIteratorValues (IterV _ _ listVal)
    | hasIterators listVal = do
        generators <- (`withLocation` getIteratorValues) listVal
        vals <- mapM getListValues generators
        return $ concat vals
    | otherwise = do
        listVal' <- withLocation listVal evaluateUpToReference
        getListValues listVal'
getIteratorValues (OperatorCall _ fid args) = do
    ann <- getCurrentLocation
    vals <- mapM (`withLocation` getIteratorValues) args
    setCurrentLocation ann
    mapM (evaluateOperator fid) $ sequence vals
getIteratorValues val = do
    val' <- evaluateUpToReference val
    return [val']


-- -----------------
-- * Evaluators

-- | Recursively solves references until it reaches a normal value.
-- If the argument is a list, a copy of it is returned.
evaluateReferences :: ReadWrite m => Bare Value -> EvaluatorEnv m (Bare Value)
evaluateReferences (VarV _ name) = do
    isDef <- variableIsDefined name
    unless isDef $ throwHere (UndefinedVariable name)
    getVariableValue name >>= evaluateReferences
evaluateReferences (RefV _ addr) = getValueAtAddress addr >>= evaluateReferences
evaluateReferences listVal@(ListV {}) = copyValue listVal
evaluateReferences val = return val

-- | Evaluates a value until a reference is reached.
-- If the argument is a list, it must be one created by extension so it is initialized.
evaluateUpToReference :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateUpToReference (VarV _ name) = do
    isDef <- variableIsDefined name
    unless isDef $ throwHere (UndefinedVariable name)
    RefV () <$> getVariableAddress name
evaluateUpToReference (OperatorCall _ fid args) = do
    ann <- getCurrentLocation
    args' <- mapM (`withLocation` evaluateUpToReference) args
    setCurrentLocation ann
    evaluateOperator fid args'
evaluateUpToReference (ListV _ elemsType vals) = do
    vals' <- mapM (`withLocation` evaluateValue) vals
    addrs <- mapM addValue vals'
    return $ ListV () elemsType (map (RefV ()) addrs)
evaluateUpToReference val = return $ void val

-- | Evaluates a value until a basic value is reached.
evaluateValue :: ReadWrite m => Annotated Value -> EvaluatorEnv m (Bare Value)
evaluateValue (IterV {}) = error "Shouldn't happen: values with iterators must be solved before evaluating them"
evaluateValue (ValueM _ _) = error "Shouldn't happen: values must be solved before evaluating them"
evaluateValue val = evaluateUpToReference val >>= evaluateReferences

-- | Evaluates a list of sentences in a new block, discarding it afterwards.
evaluateSentences :: ReadWrite m => [Annotated Sentence] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateSentences [] = return Nothing
evaluateSentences sentences = inContainedScope $ firstNotNull evaluateSentenceWithLocation sentences
    where
        evaluateSentenceWithLocation :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
        evaluateSentenceWithLocation sentence = do
            let ann = getLocation sentence
            result <- withLocation sentence evaluateSentence
            setCurrentLocation ann
            return result

evaluateSentence :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
evaluateSentence s = tick >> evaluateSentence' s
    where
        evaluateSentence' :: ReadWrite m => Annotated Sentence -> EvaluatorEnv m (Maybe (Bare Value))
        evaluateSentence' (VarDef _ ~(vn:vns) _ v) = do
            v' <- case v of
                (ListV _ et vs) -> do
                    vs' <- concat <$> mapM (getIteratorValues >=> mapM evaluateReferences) vs
                    addrs <- mapM addValue vs'
                    return $ ListV () et (map (RefV ()) addrs)
                _ -> withLocation v evaluateValue
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
        evaluateSentence' (Return _ v) = do
            v' <- withLocation v evaluateValue
            return $ Just v'
        evaluateSentence' (ProcedureCall _ fid vs) = do
            ann <- getCurrentLocation
            vss <- mapM (`withLocation` getIteratorValues) vs
            setCurrentLocation ann
            mapM_ (evaluateProcedure fid) $ sequence vss
            return Nothing
        evaluateSentence' (Try _ ss) = evaluateSentences ss `catchCodeError` \_ -> return Nothing
        evaluateSentence' (TryCatch _ ts cs) = evaluateSentences ts `catchCodeError` \_ -> evaluateSentences cs
        evaluateSentence' (Throw _ msg) = throwHere $ CodeError msg
        evaluateSentence' (SentenceM _ _) = error "Shouldn't happen: sentences must be solved before evaluating them"

evaluateOperator :: ReadWrite m => FunId -> [Bare Value] -> EvaluatorEnv m (Bare Value)
evaluateOperator fid vs = do
    r <- evaluateFunction fid vs
    case r of
        Just r' -> return r'
        Nothing -> throwHere ExpectedResult

evaluateProcedure :: ReadWrite m => FunId -> [Bare Value] -> EvaluatorEnv m ()
evaluateProcedure fid vs = void $ evaluateFunction fid vs

evaluateFunction :: ReadWrite m =>  FunId -> [Bare Value] -> EvaluatorEnv m (Maybe (Bare Value))
evaluateFunction fid vs = do
    (FunCallable (Title _ t) ss) <- getFunctionCallable fid
    vs' <- evaluateArguments t vs
    if isBuiltInOperator fid
        then Just <$> evaluateBuiltInOperator fid vs'
        else if isBuiltInProcedure fid
            then evaluateBuiltInProcedure fid vs' >> return Nothing
            else do
                (vars, refs) <- variablesFromTitle t vs'
                inNewScope (evaluateSentences ss) vars refs

--


-- Main

evaluateProgram :: ReadWrite m => Program -> m (Either Error (((), Location), EvaluatorData))
evaluateProgram prog = runEvaluatorEnv (evaluateProgram' prog) initialLocation initialState
    where
        evaluateProgram' :: ReadWrite m => Program -> EvaluatorEnv m ()
        evaluateProgram' prog = do
            registerFunctions prog
            evaluateProcedure "run" []

--
