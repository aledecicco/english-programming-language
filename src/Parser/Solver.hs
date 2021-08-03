{-# LANGUAGE TupleSections #-}

module Solver where

import Data.Char ( isUpper, toLower )
import Data.Maybe ( fromJust, catMaybes )
import Data.List ( find )
import Control.Monad ( unless, when, void )

import Utils ( getFunId, firstNotNull, allOrNone, allNotNull )
import BuiltInDefs
import SolverEnv
import AST
import Errors

--



-- Auxiliary

-- Returns all the ways to split a list in two, with at least one element in the first part
splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = splits' [x] xs
    where
        splits' :: [a] -> [a] -> [([a], [a])]
        splits' l [] = [(l,[])]
        splits' l (r:rs) = (l, r:rs) : splits' (l++[r]) rs

-- Returns whether a list of words is a prefix of the given matchable, and the unmatched sufix
isPrefix :: [String] -> [MatchablePart a] -> (Bool, [MatchablePart a])
isPrefix [] ms = (True, ms)
isPrefix _ [] = (False, [])
isPrefix (t:ts) ms@(WordP _ w : ps)
    | t == w = isPrefix ts ps
    | otherwise = (False, ms)
isPrefix _ ms = (False, ms)

-- Returns all the ways a list of matchables can be used to fill the gaps (parameters) of a title
sepByTitle :: [MatchablePart a] -> [TitlePart b] -> [[[MatchablePart a]]]
sepByTitle [] [] = [[]]
sepByTitle _ [] = []
sepByTitle ps (TitleWords _ ws : ts) =
    let (isP, rest) = ws `isPrefix` ps
    in if isP then sepByTitle rest ts else []
sepByTitle ps (TitleParam {} : ts) = do
    (span, rest) <- splits ps
    restMatches <- sepByTitle rest ts
    return $ span:restMatches

-- Returns the matchables in a list before and after a given matchable
splitBy :: [MatchablePart a] -> MatchablePart b -> ([MatchablePart a], [MatchablePart a])
splitBy [] _ = ([], [])
splitBy (p:ps) p' =
    if void p == void p'
        then ([], ps)
        else
            let (bef, aft) = splitBy ps p'
            in (p:bef, aft)

--


-- Auxiliary matchers

matchAsName :: [MatchablePart a] -> SolverEnv (Maybe Name)
matchAsName [WordP _ w] = return $ Just [w]
matchAsName (WordP _ w : ps) = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ Just (w : ws)
        Nothing -> return Nothing
matchAsName _ = return Nothing

matchAsFunctionCall :: [Annotated MatchablePart] -> [FunSignature] -> SolverEnv [(FunId, [Annotated Value])]
matchAsFunctionCall ps fs = concat <$> mapM (matchAsFunctionCall' ps) fs
    where
        matchAsFunctionCall' :: [Annotated MatchablePart] -> FunSignature -> SolverEnv [(FunId, [Annotated Value])]
        matchAsFunctionCall' ps (FunSignature (Title _ ft) _) = do
            let posParams = sepByTitle ps ft
                fid = getFunId ft
            paramMatches <- allNotNull matchAllParams posParams
            let paramCombs = map sequence paramMatches
            return $ concatMap (map (fid,)) paramCombs

        matchAllParams :: [[Annotated MatchablePart]] -> SolverEnv (Maybe [[Annotated Value]])
        matchAllParams pss = do
            r <- mapM matchAsValue pss
            if any null r
                then return Nothing
                else return $ Just r

--


-- Value matchers

matchAsPrimitive :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsPrimitive [IntP ann n] = return [IntV ann n]
matchAsPrimitive [FloatP ann n] = return [FloatV ann n]
matchAsPrimitive [CharP ann c] = return [CharV ann c]
matchAsPrimitive [StringP ann@(ln, cn) s] =
    let cns = [cn+1 ..]
        lvs = zip s cns
        locateC = \(c, cn') -> CharV (ln, cn') c
    in return [ListV ann CharT $ map locateC lvs]
matchAsPrimitive [WordP ann w]
    | w == "true" = return [BoolV ann True]
    | w == "false" = return [BoolV ann False]
matchAsPrimitive _ = return []

matchAsVariable :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsVariable ps = do
    r <- matchAsName ps
    let posN = case r of
            Just n@("the":ps) -> [n, ps]
            Just n -> [n]
            Nothing -> []
        ann = getFirstLocation ps
    ns <- allNotNull matchAsVariable' posN
    return $ map (VarV ann) ns
    where
        matchAsVariable' :: Name -> SolverEnv (Maybe Name)
        matchAsVariable' n = do
            isDef <- variableIsDefined n
            if isDef
                then return $ Just n
                else return Nothing

matchAsOperatorCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsOperatorCall ps = do
    let ann = getFirstLocation ps
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall ps operators
    return $ map (uncurry $ OperatorCall ann) r

matchAsIterator :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsIterator (WordP ann "each":ps) = do
    let (bef, aft) = splitBy ps (WordP () "in")
    r <- matchAsName bef
    case r of
        Just _ -> do
            r <- matchAsValue aft
            return $ IterV ann <$> r
        Nothing -> return []
matchAsIterator _ = return []

matchAsValue :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = do
    r <- mapM (\matcher -> matcher ps) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall, matchAsIterator]
    return $ concat r

--


-- Sentence matchers

matchAsProcedureCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsProcedureCall ps = do
    procedures <- getProcedureSignatures
    r <- matchAsFunctionCall ps procedures
    r' <- case ps of
        (WordP fAnn (x:xs) : ps) ->
            if isUpper x
                then do
                    let lowerCaseTitle = WordP fAnn (toLower x : xs) : ps
                    matchAsFunctionCall lowerCaseTitle procedures
                else return []
        _ -> return []
    let ann = getFirstLocation ps
    return $ map (uncurry  $ ProcedureCall ann) (r ++ r')

matchAsSentence :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsSentence [ParensP ps] = matchAsSentence ps
matchAsSentence ps = matchAsProcedureCall ps

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
getValueType (IterV _ lv) = do
    lt <- getValueType lv
    let et = ListT $ AnyT "a"
    if lt `satisfiesType` et
        then return $ getElementsType lt
        else throwHere $ WrongTypeValue et lt
getValueType (ValueM _ _) = error "Shouldn't happen: values must be solved before getting their types"
getValueType (RefV _ _) = error "Shouldn't happen: references can't exist before evaluating"

getElementsType :: Type -> Type
getElementsType (RefT t) = getElementsType t
getElementsType (ListT t) = t
getElementsType IntT = error "Shouldn't happen: ints don't contain types"
getElementsType FloatT = error "Shouldn't happen: floats don't contain types"
getElementsType BoolT = error "Shouldn't happen: bools don't contain types"
getElementsType CharT = error "Shouldn't happen: chars don't contain types"
getElementsType (AnyT _) = error "Shouldn't happen: generic types don't contain types"

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
    getParameterTypes' [] ft vs
    where
        -- Returns the type of all arguments given a list of type bindings
        getParameterTypes' :: [(String, Type)] -> [Bare TitlePart] -> [Annotated Value] -> SolverEnv [Type]
        getParameterTypes' _ _ [] = return []
        getParameterTypes' bts (TitleWords {} : tps) vs = getParameterTypes' bts tps vs
        getParameterTypes' bts (TitleParam _ n pt : tps) (v:vs) = do
            ann <- getCurrentLocation
            at <- withLocation v getValueType
            unless (at `satisfiesType` pt) $ throwHere (WrongTypeParameter pt at n)
            let at' = solveTypeReferences pt at
            r <- case solveTypeBindings pt at' of
                Just (tF, (tid, bt)) ->
                    case lookup tid bts of
                        Just bt' -> do
                            unless (bt `satisfiesType` bt') $ throwHere (WrongTypeParameter (tF bt') at' n)
                            getParameterTypes' bts tps vs
                        Nothing -> getParameterTypes' ((tid, bt):bts) tps vs
                Nothing -> getParameterTypes' bts tps vs
            setCurrentLocation ann
            return (at':r)
        getParameterTypes' _ [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

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

--


-- Registration

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
        registerParameter (TitleParam ann vn t) = setNewVariableType vn t
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
                (v:_) -> {- ToDo: ambiguity warning >> -} return v
                [] -> throwHere $ UnmatchableValueTypes ps
solveValue valFun l@((ListV ann eT es)) = do
    valFun l
    es' <- mapM (`withLocation` solveValueWithType eT) es
    return $ ListV ann eT es'
solveValue valFun v = valFun v >> return v

solveValueWithType :: Type -> Annotated Value -> SolverEnv (Annotated Value)
solveValueWithType t = solveValue (`checkValueType` t)

solveValueWithAnyType :: Annotated Value -> SolverEnv (Annotated Value)
solveValueWithAnyType = solveValue (void . getValueType)

solveSentence :: Maybe Type -> Annotated Sentence -> SolverEnv (Annotated Sentence)
solveSentence _ (VarDef ann vNs (Just t) v) = do
    mapM_ (`setNewVariableType` t) vNs
    v' <- withLocation v (solveValueWithType t)
    return $ VarDef ann vNs (Just t) v'
solveSentence _ (VarDef ann vNs Nothing v) = do
    v' <- withLocation v solveValueWithAnyType
    t <- getValueType v'
    mapM_ (`setNewVariableType` t) vNs
    return $ VarDef ann vNs Nothing v'
solveSentence rt (If ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT
    ls' <- solveSentences ls rt
    return $ If ann v' ls'
solveSentence rt (IfElse ann v lsT lsF) = do
    v' <- withLocation v $ solveValueWithType BoolT
    lsT' <- solveSentences lsT rt
    lsF' <- solveSentences lsF rt
    return $ IfElse ann v' lsT' lsF'
solveSentence rt (ForEach ann iN iT v ls) = do
    v' <- withLocation v $ solveValueWithType (ListT iT)
    setNewVariableType iN (RefT iT)
    ls' <- solveSentences ls rt
    removeVariableType iN
    return $ ForEach ann iN iT v' ls'
solveSentence rt (Until ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT
    ls' <- solveSentences ls rt
    return $ Until ann v' ls'
solveSentence rt (While ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT
    ls' <- solveSentences ls rt
    return $ While ann v' ls'
solveSentence rt (Result ann v) =
    case rt of
        Just t -> do
            v' <- withLocation v $ solveValueWithType t
            return $ Result ann v'
        Nothing -> throwHere ResultInProcedure
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
                (s:_) -> {- ToDo: ambiguity warning >> -} return s
                [] -> throwHere $ UnmatchableSentenceTypes ps
solveSentence _ (ProcedureCall {}) = error "Shouldn't happen: procedure calls can only be created by solving a matchable"

solveSentences :: [Annotated Sentence] -> Maybe Type -> SolverEnv [Annotated Sentence]
solveSentences ss rt = mapM (`withLocation` solveSentence rt) ss

solveBlock :: Annotated Block -> SolverEnv (Annotated Block)
solveBlock (FunDef ann t rt ss) = do
    withLocation t registerParameters
    FunDef ann t rt <$> solveSentences ss rt

--


-- Main

solveProgram :: Program -> Either Error ((Program, Location), SolverData)
solveProgram p = runSolverEnv (solveProgram' p) initialState initialLocation
    where
        solveProgram' :: Program -> SolverEnv Program
        solveProgram' p = do
            setFunctions $ builtInOperators ++ builtInProcedures
            registerFunctions p
            mainIsDef <- functionIsDefined "run"
            unless mainIsDef $ throwError (Error Nothing (UndefinedFunction "run"))
            mapM (\b -> solveBlock b <* resetVariables) p

--
