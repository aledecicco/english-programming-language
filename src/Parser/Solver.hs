{-# LANGUAGE TupleSections #-}

module Solver where

import Data.Char ( isUpper, toLower )
import Data.Maybe ( fromJust, isNothing )
import Data.List ( find )
import Control.Monad ( unless, when, void )

import Utils ( getFunId, firstNotNull, allOrNone )
import BuiltInDefs
import ParserEnv
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

--


-- Auxiliary matchers

matchAsName :: [MatchablePart a] -> ParserEnv (Maybe Name)
matchAsName [WordP _ w] = return $ Just [w]
matchAsName (WordP _ w : ps) = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ Just (w : ws)
        Nothing -> return Nothing
matchAsName _ = return Nothing

-- Matches a list of matchables as a call to one of the given functions
matchAsFunctionCall :: [Annotated MatchablePart] -> [FunSignature] -> ParserEnv (Maybe (FunId, [Annotated Value]))
matchAsFunctionCall ps fs = do
    firstNotNull matchAsFunctionCall' fs
    where
        matchAsFunctionCall' :: FunSignature -> ParserEnv (Maybe (FunId, [Annotated Value]))
        matchAsFunctionCall' (FunSignature (Title _ ft) _) = do
            let posParams = sepByTitle ps ft
                fid = getFunId ft
            r <- firstNotNull matchAllParams posParams
            return $ (fid, ) <$> r
        matchAllParams :: [[Annotated MatchablePart]] -> ParserEnv (Maybe [Annotated Value])
        matchAllParams = allOrNone matchAsValue

--


-- Value matchers

matchAsPrimitive :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsPrimitive [IntP ann n] = return $ Just (IntV ann n)
matchAsPrimitive [FloatP ann n] = return $ Just (FloatV ann n)
matchAsPrimitive [CharP ann c] = return $ Just (CharV ann c)
matchAsPrimitive [StringP ann@(ln, cn) s] =
    let cns = [cn+1 ..]
        lvs = zip s cns
        locateC = \(c, cn') -> CharV (ln, cn') c
    in return $ Just (ListV ann CharT $ map locateC lvs)
matchAsPrimitive [WordP ann w]
    | w == "true" = return $ Just (BoolV ann True)
    | w == "false" = return $ Just (BoolV ann False)
matchAsPrimitive _ = return Nothing

matchAsVariable :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsVariable ps = do
    let ann = getFirstLocation ps
    r <- matchAsName ps
    case r of
        Just n -> do
            isDef <- variableIsDefined n
            matchAsVariable' n isDef ann
        Nothing -> return Nothing
    where
        matchAsVariable' :: Name -> Bool -> Location -> ParserEnv (Maybe (Annotated Value))
        matchAsVariable' n True ann = return $ Just (VarV ann n)
        matchAsVariable' ("the":n) False ann = do
            isDef <- variableIsDefined n
            if isDef
                then return $ Just (VarV ann n)
                else return Nothing
        matchAsVariable' _ _ _ = return Nothing

matchAsOperatorCall :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsOperatorCall ps = do
    let ann = getFirstLocation ps
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall ps operators
    return $ uncurry (OperatorCall ann) <$> r

matchAsValue :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = firstNotNull (\matcher -> matcher ps) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall]

--


-- Sentence matchers

matchAsProcedureCall :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Sentence))
matchAsProcedureCall ps = do
    procedures <- getProcedureSignatures
    r <- matchAsFunctionCall ps procedures
    matchAsProcedureCall' ps procedures r
    where
        -- Takes the result of matching as a function call, tries again if necessary, and returns a procedure call
        matchAsProcedureCall' :: [Annotated MatchablePart] -> [FunSignature] -> Maybe (FunId, [Annotated Value]) -> ParserEnv (Maybe (Annotated Sentence))
        matchAsProcedureCall' (WordP fAnn (x:xs) : ps) procedures Nothing
            | isUpper x = do
                let lowerCaseTitle = WordP fAnn (toLower x : xs) : ps
                r <- matchAsFunctionCall lowerCaseTitle procedures
                return $ uncurry (ProcedureCall fAnn) <$> r
            | otherwise = return Nothing
        matchAsProcedureCall' ps _ r = do
            let ann = getFirstLocation ps
            return $ uncurry (ProcedureCall ann) <$> r


matchAsSentence :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Sentence))
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

getValueType :: Value a -> ParserEnv Type
getValueType (IntV _ _) = return IntT
getValueType (FloatV _ _) = return FloatT
getValueType (BoolV _ _) = return BoolT
getValueType (CharV _ _) = return CharT
getValueType (ListV _ t _) = return $ ListT t
getValueType (VarV _ n) = RefT . fromJust <$> getVariableType n
getValueType (OperatorCall _ fid vs) = do
    vTs <- mapM getValueType vs
    getOperatorCallType fid vTs
    where
        getOperatorCallType :: FunId -> [Type] -> ParserEnv Type
        getOperatorCallType fid vTs = do
            ~(FunSignature _ (Operator tFun)) <- fromJust <$> getFunctionSignature fid
            return $ tFun vTs

getIteratorType :: Type -> Type
getIteratorType (RefT t) = getIteratorType t
getIteratorType (ListT t) = t

--


-- Validations

setVariableTypeWithCheck :: Name -> Type -> ParserEnv ()
setVariableTypeWithCheck vn t = do
    r <- getVariableType vn
    case r of
        Just t' -> unless (t `satisfiesType` t') $ throwHere (MismatchingTypeAssigned t' t vn)
        Nothing -> setVariableType vn t

setNewVariableType :: Name -> Type -> ParserEnv ()
setNewVariableType vn t' = do
    isDef <- variableIsDefined vn
    if isDef
        then throwHere $ VariableAlreadyDefined vn
        else setVariableType vn t'

checkValueType :: Annotated Value -> Type -> ParserEnv ()
checkValueType v t = do
    t' <- withLocation v getValueType
    unless (t' `satisfiesType` t) $ throwHere (WrongTypeValue t t')

-- Validates that a value is correctly formed
checkValueIntegrity :: Annotated Value -> ParserEnv ()
checkValueIntegrity (OperatorCall _ fid vs) = checkFunctionCallIntegrity (fid, vs)
checkValueIntegrity (ListV _ t vs) = mapM_ checkElement vs
    where
        checkElement :: Annotated Value -> ParserEnv ()
        checkElement v = withLocation v checkValueIntegrity >> checkValueType v t
checkValueIntegrity _ = return ()

checkFunctionCallIntegrity :: (FunId, [Annotated Value]) -> ParserEnv ()
checkFunctionCallIntegrity (fid, vs) = do
    mapM_ (`withLocation` checkValueIntegrity) vs
    ~(FunSignature (Title _ ft) _) <- fromJust <$> getFunctionSignature fid
    checkParameterTypes [] ft vs
    where
        checkParameterTypes :: [(String, Type)] -> [Bare TitlePart] -> [Annotated Value] -> ParserEnv ()
        checkParameterTypes _ _ [] = return ()
        checkParameterTypes bts (TitleWords {} : ts) vs = checkParameterTypes bts ts vs
        checkParameterTypes bts (TitleParam _ n t : ts) (v:vs) = do
            t' <- withLocation v getValueType
            case findTypeToBind t of
                Just tid ->
                    case find (\bt -> fst bt == tid) bts of
                        Just (_, t) ->
                            if t' `satisfiesType` t
                                then checkParameterTypes bts ts vs
                                else throwHere $ WrongTypeParameter t t' n
                        Nothing -> checkParameterTypes ((tid, t'):bts) ts vs
                Nothing ->
                    if t' `satisfiesType` t
                        then checkParameterTypes bts ts vs
                        else throwHere $ WrongTypeParameter t t' n

        findTypeToBind :: Type -> Maybe String
        findTypeToBind (ListT t) = findTypeToBind t
        findTypeToBind (RefT t) = findTypeToBind t
        findTypeToBind (AnyT tid) = Just tid
        findTypeToBind _ = Nothing

--


-- Registration

registerFunctions :: Program -> ParserEnv ()
registerFunctions = mapM_ (`withLocation` registerFunction)
    where
        registerFunction :: Annotated Block -> ParserEnv ()
        registerFunction (FunDef _ t@(Title _ ft) rt _) = do
            let fid = getFunId ft
            isDef <- functionIsDefined fid
            when isDef $ throwHere (FunctionAlreadyDefined fid)
            let frt = case rt of
                    Just rt -> Operator $ const rt
                    Nothing -> Procedure
            setFunctionSignature fid $ FunSignature (void t) frt

registerParameters :: Annotated Title -> ParserEnv ()
registerParameters (Title _ ts) = mapM_ (`withLocation` registerParameter) ts
    where
        registerParameter :: Annotated TitlePart -> ParserEnv ()
        registerParameter (TitleParam ann vn t) = setNewVariableType vn t
        registerParameter _ = return ()

--


-- Solvers

solveValue :: Annotated Value -> ParserEnv (Annotated Value)
solveValue (ListV ann t es) = do
    es' <- mapM (`withLocation` solveValueWithType t) es
    return $ ListV ann t es'
solveValue (ValueM _ ps) = do
    r <- matchAsValue ps
    case r of
        Just v' -> checkValueIntegrity v' >> return v'
        Nothing -> throwHere $ UnmatchableValue ps
solveValue v = return v

solveValueWithType :: Type -> Annotated Value -> ParserEnv (Annotated Value)
solveValueWithType t v = do
    v' <- solveValue v
    checkValueType v' t
    return v'

solveSentence :: Maybe Type -> Annotated Sentence -> ParserEnv (Annotated Sentence)
solveSentence _ (VarDef ann vNs v) = do
    v' <- withLocation v solveValue
    t <- getValueType v'
    mapM_ (`setVariableTypeWithCheck` t) vNs
    return $ VarDef ann vNs v'
solveSentence rt (If ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT
    ls' <- solveSentences ls rt
    return $ If ann v' ls'
solveSentence rt (IfElse ann v lsT lsF) = do
    v' <- withLocation v $ solveValueWithType BoolT
    lsT' <- solveSentences lsT rt
    lsF' <- solveSentences lsF rt
    return $ IfElse ann v' lsT' lsF'
solveSentence rt (ForEach ann iN v ls) = do
    v' <- withLocation v $ solveValueWithType (ListT $ AnyT "a")
    iT <- getIteratorType <$> getValueType v'
    setNewVariableType iN iT
    ls' <- solveSentences ls rt
    removeVariableType iN
    return $ ForEach ann iN v' ls'
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
        Just s@(ProcedureCall ann fid vs) -> checkFunctionCallIntegrity (fid, vs) >> return s
        _ -> throwHere $ UnmatchableSentence ps

solveSentences :: [Annotated Sentence] -> Maybe Type -> ParserEnv [Annotated Sentence]
solveSentences ss rt = mapM (`withLocation` solveSentence rt) ss

solveBlock :: Annotated Block -> ParserEnv (Annotated Block)
solveBlock (FunDef ann t rt ss) = do
    withLocation t registerParameters
    FunDef ann t rt <$> solveSentences ss rt

--


-- Main

solveProgram :: Program -> Either Error ((Program, Location), ParserData)
solveProgram p = runParserEnv (solveProgram' p) initialState initialLocation
    where
        solveProgram' :: Program -> ParserEnv Program
        solveProgram' p = do
            setFunctions $ builtInOperators ++ builtInProcedures
            registerFunctions p
            mainIsDef <- functionIsDefined "run"
            unless mainIsDef $ throw (UndefinedFunction "run")
            mapM (\b -> solveBlock b <* resetVariables) p

--
