module Solver where

import Data.Maybe ( fromJust, catMaybes )
import Data.List ( find )
import Control.Monad ( unless, when, void )
import Control.Monad.Trans.State ( gets, modify, runState, State )
import qualified Data.Map.Strict as M

import Utils ( allNotNull, hasIterators, typeName, getFunId )
import Matchers
import BuiltInDefs
import SolverEnv
import AST
import Errors

--



-- Aliases

possibleAliases :: Type -> [Name]
possibleAliases (RefT t) = possibleAliases t
possibleAliases (ListT t) =
    let as = ["list"] : map (["list", "of"]++) (possibleAliases' t)
    in if t == CharT then ["string"]:as else as
    where
        possibleAliases' :: Type -> [Name]
        possibleAliases' (ListT t@(ListT _)) = map (["lists", "of"]++) (possibleAliases' t)
        possibleAliases' (ListT t) =
            let as = ["lists"] :  map (["lists", "of"]++) (possibleAliases' t)
            in if t == CharT then ["strings"]:as else as
        possibleAliases' t  = [typeName t True]
possibleAliases t =  [typeName t False]

intToPosition :: Int -> String
intToPosition 1 = "1st"
intToPosition 2 = "2nd"
intToPosition 3 = "3rd"
intToPosition n
    | n < 10 = show n ++ "th"
    | otherwise = show (div n 10) ++ intToPosition (mod n 10)

titleTypes :: [TitlePart a] -> [Type]
titleTypes [] = []
titleTypes (TitleWords _ _ : ts) = titleTypes ts
titleTypes (TitleParam _ _ t : ts) = t : titleTypes ts

parameterNames :: [TitlePart a] -> [Name]
parameterNames [] = []
parameterNames (TitleWords _ _ : ts) = parameterNames ts
parameterNames (TitleParam _ ns _ : ts) = ns ++ parameterNames ts

computeAliases :: [TitlePart a] -> [[Name]]
computeAliases ts =
    let types = titleTypes ts
        setNames = parameterNames ts
        posAs = map possibleAliases types
        (_, totAs) = runState (countRepetitions (concat posAs) >> cancelNames setNames) M.empty
        (as, _) = runState (mapM (`getParameterAliases` totAs) posAs) M.empty
    in as
    where
        countRepetitions :: [Name] -> State (M.Map Name Int) ()
        countRepetitions = mapM_ (\n -> modify $ M.insertWith (+) n 1)

        cancelNames :: [Name] -> State (M.Map Name Int) ()
        cancelNames = mapM_ $ \(w:ws) -> do
            modify $ M.insert (w:ws) 0
            unless (w == "the") $ modify (M.insert ("the":w:ws) 0)

        getParameterAliases :: [Name] -> M.Map Name Int -> State (M.Map Name Int) [Name]
        getParameterAliases ns totAs = do
            countRepetitions ns
            ns' <- mapM (`addOrdinal` totAs) ns
            return $ filter (\n -> not $ M.member n totAs) ns'

        addOrdinal :: Name -> M.Map Name Int -> State (M.Map Name Int) Name
        addOrdinal n totAs = do
            let totReps = totAs M.! n
            if totReps > 1
                then do
                    rep <- gets (M.! n)
                    return $ "the" : intToPosition rep : n
                else return $ "the" : n

addAliases :: [TitlePart a] -> [TitlePart a]
addAliases ts = addAliases' ts $ computeAliases ts
    where
        addAliases' :: [TitlePart a] -> [[Name]] -> [TitlePart a]
        addAliases' ts [] = ts
        addAliases' (TitleWords ann ws : ts) as = TitleWords ann ws : addAliases' ts as
        addAliases' (TitleParam ann [] t : ts) (a:as) = TitleParam ann a t : addAliases' ts as
        addAliases' (TitleParam ann ns t : ts) (_:as) = TitleParam ann ns t : addAliases' ts as
        addAliases' [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of types"

--


-- Types information

satisfiesType :: Type -> Type -> Bool
satisfiesType _ (AnyT _) = True
satisfiesType IntT FloatT = True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType (RefT t1) (RefT t2) = t1 `satisfiesType` t2
satisfiesType (RefT t1) t2 = t1 `satisfiesType` t2
satisfiesType t1 t2 = t1 == t2

getValueType :: Annotated Value -> SolverEnv Type
getValueType (IntV _ _) = return IntT
getValueType (FloatV _ _) = return FloatT
getValueType (BoolV _ _) = return BoolT
getValueType (CharV _ _) = return CharT
getValueType (ListV _ t _) = return $ ListT t
getValueType (VarV _ n) = RefT . fromJust <$> getVariableType n
getValueType (OperatorCall _ fid vs) = do
    ~(FunSignature _ (Operator tFun)) <- fromJust <$> getFunctionSignature fid
    vTs <- getParameterTypesWithCheck (fid, vs)
    return $ tFun vTs
getValueType (IterV _ t lv) = do
    lt <- withLocation lv getValueType
    let et = ListT t
    if lt `satisfiesType` et
        then return $ RefT (getElementsType lt)
        else throwHere $ WrongTypeValue et lt
getValueType (ValueM _ _) = error "Shouldn't happen: values must be solved before getting their types"
getValueType (RefV _ _) = error "Shouldn't happen: references can't exist before evaluating"

getElementsType :: Type -> Type
getElementsType (RefT t) = getElementsType t
getElementsType (ListT t) = t
getElementsType _ = error "Shouldn't happen: type doesn't contain types"

--


-- Validations

setVariableTypeWithCheck :: Name -> Type -> SolverEnv ()
setVariableTypeWithCheck vn t = do
    r <- getVariableType vn
    case r of
        Just t' -> unless (t `satisfiesType` t') $ throwHere (MismatchingTypeAssigned t' t vn)
        Nothing -> setVariableType vn t

setNewVariableType :: Name -> Type -> SolverEnv ()
setNewVariableType vn t' = do
    isDef <- variableIsDefined vn
    when isDef $ throwHere (VariableAlreadyDefined vn)
    setVariableType vn t'

checkValueType :: Annotated Value -> Type -> SolverEnv ()
checkValueType v t = do
    t' <- withLocation v getValueType
    unless (t' `satisfiesType` t) $ throwHere (WrongTypeValue t t')

checkProcedureCallType :: Annotated Sentence -> SolverEnv ()
checkProcedureCallType (ProcedureCall ann fid vs) = void $ getParameterTypesWithCheck (fid, vs)
checkProcedureCallType _ = error "Shouldn't happen: sentence given is not a procedure call"

getParameterTypesWithCheck :: (FunId, [Annotated Value]) -> SolverEnv [Type]
getParameterTypesWithCheck (fid, vs) = do
    ~(FunSignature (Title _ ft) _) <- fromJust <$> getFunctionSignature fid
    getParameterTypes' [] ft vs 0
    where
        -- Returns the type of all arguments given a list of type bindings
        getParameterTypes' :: [(String, Type)] -> [Bare TitlePart] -> [Annotated Value] -> Int -> SolverEnv [Type]
        getParameterTypes' _ _ [] _ = return []
        getParameterTypes' bts (TitleWords {} : tps) vs pos = getParameterTypes' bts tps vs pos
        getParameterTypes' bts (TitleParam _ n pt : tps) (v:vs) pos = do
            ann <- getCurrentLocation
            at <- withLocation v getValueType
            unless (at `satisfiesType` pt) $ throwHere (WrongTypeParameter pt at pos fid)
            let at' = solveTypeReferences pt at
            r <- case solveTypeBindings pt at' of
                Just (tF, (tid, bt)) ->
                    case lookup tid bts of
                        Just bt' -> do
                            unless (bt `satisfiesType` bt') $ throwHere (WrongTypeParameter (tF bt') at' pos fid)
                            getParameterTypes' bts tps vs (pos+1)
                        Nothing -> getParameterTypes' ((tid, bt):bts) tps vs (pos+1)
                Nothing -> getParameterTypes' bts tps vs (pos+1)
            setCurrentLocation ann
            return (at':r)
        getParameterTypes' _ [] (_:_) _ = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

        -- Returns the type bound by the given parameter and argument, and a function to build the whole type
        solveTypeBindings :: Type -> Type -> Maybe (Type -> Type, (String, Type))
        solveTypeBindings (ListT et) (ListT at) = solveTypeBindings et at >>= (\(tF, bt) -> Just (ListT . tF, bt))
        solveTypeBindings (RefT et) (RefT at) = solveTypeBindings et at >>= (\(tF, bt) -> Just (RefT . tF, bt))
        solveTypeBindings (AnyT tid) at = Just (id, (tid, at))
        solveTypeBindings _ _ = Nothing

        -- Solves a reference if the type it contains is expected by the given parameter
        solveTypeReferences :: Type -> Type -> Type
        solveTypeReferences (RefT t1) (RefT t2) = RefT $ solveTypeReferences t1 t2
        solveTypeReferences t1 (RefT t2) = solveTypeReferences t1 t2
        solveTypeReferences (ListT t1) (ListT t2) = ListT $ solveTypeReferences t1 t2
        solveTypeReferences _ t = t

checkNoIterators :: Annotated Value -> SolverEnv ()
checkNoIterators v = when (hasIterators v) $ throwHere ForbiddenIteratorUsed

--


-- Registration

registerAliases :: Program -> SolverEnv Program
registerAliases = return . map (\(FunDef ann (Title ann' ft) rt ss) -> FunDef ann (Title ann' $ addAliases ft) rt ss)

registerFunctions :: Program -> SolverEnv ()
registerFunctions = mapM_ (`withLocation` registerFunction)
    where
        registerFunction :: Annotated Block -> SolverEnv ()
        registerFunction (FunDef _ t@(Title _ ft) rt _) = do
            let fid = getFunId ft
            isDef <- functionIsDefined fid
            when isDef $ throwHere (FunctionAlreadyDefined fid)
            let frt = maybe Procedure (Operator . const) rt
            setFunctionSignature fid $ FunSignature (void t) frt

registerParameters :: Annotated Title -> SolverEnv ()
registerParameters (Title _ ts) = mapM_ (`withLocation` registerParameter) ts
    where
        registerParameter :: Annotated TitlePart -> SolverEnv ()
        registerParameter (TitleParam ann vns t) = mapM_ (`setNewVariableType` t) vns
        registerParameter _ = return ()

--


-- Solvers

solveValue :: (Annotated Value -> SolverEnv ()) -> Annotated Value -> SolverEnv (Annotated Value)
solveValue valFun (ValueM _ ps) = do
    r <- matchAsValue ps
    case r of
        [] -> throwHere $ UnmatchableValue ps
        [v] -> valFun v >> return v
        vs -> do
            let tryValue = (\v -> (valFun v >> return (Just v)) `catchError` (\_ -> return Nothing))
            r <- catMaybes <$> mapM tryValue vs
            case r of
                [v] -> return v
                [] -> throwHere $ UnmatchableValueTypes ps
                vs -> do
                    warnHere $ AmbiguousValue (length vs) ps
                    return $ head vs
solveValue valFun l@((ListV ann eT es)) = do
    valFun l
    es' <- mapM (`withLocation` solveValueWithType eT True) es
    return $ ListV ann eT es'
solveValue valFun v = valFun v >> return v

solveValueWithType :: Type -> Bool -> Annotated Value -> SolverEnv (Annotated Value)
solveValueWithType t allowIters v = do
    v' <- solveValue (`checkValueType` t) v
    unless allowIters $ withLocation v' checkNoIterators
    return v'

solveValueWithAnyType :: Annotated Value -> SolverEnv (Annotated Value)
solveValueWithAnyType v = do
    ann <- getCurrentLocation
    v' <- solveValue (void . getValueType) v
    setCurrentLocation ann
    checkNoIterators v'
    return v'

solveSentence :: Maybe Type -> Annotated Sentence -> SolverEnv (Annotated Sentence)
solveSentence _ (VarDef ann vNs (Just t) v) = do
    v' <- case v of
        (ListV {}) -> withLocation v (solveValueWithType t True)
        _ -> withLocation v (solveValueWithType t False)
    mapM_ (`setNewVariableType` t) vNs
    return $ VarDef ann vNs (Just t) v'
solveSentence _ (VarDef ann vNs Nothing v) = do
    v' <- withLocation v solveValueWithAnyType
    t <- getValueType v'
    mapM_ (`setNewVariableType` t) vNs
    return $ VarDef ann vNs Nothing v'
solveSentence rt (If ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls rt
    return $ If ann v' ls'
solveSentence rt (IfElse ann v lsT lsF) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    lsT' <- solveSentences lsT rt
    lsF' <- solveSentences lsF rt
    return $ IfElse ann v' lsT' lsF'
solveSentence rt (ForEach ann iN iT v ls) = do
    v' <- withLocation v $ solveValueWithType (ListT iT) False
    setNewVariableType iN (RefT iT)
    ls' <- solveSentences ls rt
    removeVariableType iN
    return $ ForEach ann iN iT v' ls'
solveSentence rt (Until ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls rt
    return $ Until ann v' ls'
solveSentence rt (While ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls rt
    return $ While ann v' ls'
solveSentence rt (Return ann v) =
    case rt of
        Just t -> do
            v' <- withLocation v $ solveValueWithType t False
            return $ Return ann v'
        Nothing -> throwHere ResultInProcedure
solveSentence rt (Try ann ss) = Try ann <$> mapM (`withLocation` solveSentence rt) ss
solveSentence rt (TryCatch ann ts cs) = do
    ts' <- mapM (`withLocation` solveSentence rt) ts
    cs' <- mapM (`withLocation` solveSentence rt) cs
    return $ TryCatch ann ts' cs'
solveSentence rt (Throw ann msg) = return $ Throw ann msg
solveSentence _ (SentenceM ann ps) = do
    r <- matchAsSentence ps
    case r of
        [] -> throwHere $ UnmatchableSentence ps
        [s] -> checkProcedureCallType s >> return s
        vs -> do
            let trySentence = (\s -> (checkProcedureCallType s >> return (Just s)) `catchError` (\_ -> return Nothing))
            r <- catMaybes <$> mapM trySentence vs
            case r of
                [s] -> return s
                [] -> throwHere $ UnmatchableSentenceTypes ps
                ss -> do
                    warnHere $ AmbiguousSentence (length ss) ps
                    return $ head ss
solveSentence _ (ProcedureCall {}) = error "Shouldn't happen: procedure calls can only be created by solving a matchable"

solveSentences :: [Annotated Sentence] -> Maybe Type -> SolverEnv [Annotated Sentence]
solveSentences ss rt = restoringVariables $ mapM (`withLocation` solveSentence rt) ss

solveBlock :: Annotated Block -> SolverEnv (Annotated Block)
solveBlock (FunDef ann t rt ss) = do
    withLocation t registerParameters
    FunDef ann t rt <$> solveSentences ss rt

--


-- Main

--ToDo: should it return function callables?
solveProgram :: Program -> (Either Error ((Program, Location), SolverData), [Warning])
solveProgram p = runSolverEnv (solveProgram' p) [] initialLocation initialState
    where
        solveProgram' :: Program -> SolverEnv Program
        solveProgram' p = do
            p' <- registerAliases p
            setFunctions $ builtInOperators ++ builtInProcedures
            registerFunctions p'
            mainIsDef <- functionIsDefined "run"
            unless mainIsDef $ throwNowhere (UndefinedFunction "run")
            mapM (restoringVariables . solveBlock) p'

--
