{-# LANGUAGE LiberalTypeSynonyms #-}
{-|
Module      : Solver
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The language's second phase of parsing.
Transforms the parts left unparsed by the "FuzzyParser" into concrete Values or Sentences.
-}

module Solver where

import Control.Monad (unless, void, when)
import Control.Monad.Trans.State (gets, modify, runState, State)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Map.Strict as M

import AST
import Errors
import Matchers
import SolverEnv
import Utils (getFunId, hasIterators, typeName)


-- -----------------
-- * Aliases generation

-- | Returns all the possible ways to name a parameter of a given type.
--
-- >>> possibleAliases (ListT (ListT (ListT CharT)))
-- [["list"],["list","of","lists","of","strings"],["list","of","lists","of","lists"],["list","of","lists","of","lists","of","characters"]]
possibleAliases :: Type -> [Name]
possibleAliases (RefT refType) = possibleAliases refType
possibleAliases (ListT elemsType) =
    let as = ["list"] : map (["list", "of"]++) (possibleAliases' elemsType)
    in if elemsType == CharT then ["string"]:as else as
    where
        possibleAliases' :: Type -> [Name]
        possibleAliases' (ListT elemsType@(ListT _)) = map (["lists", "of"]++) (possibleAliases' elemsType)
        possibleAliases' (ListT elemsType) =
            let aliases = ["lists"] :  map (["lists", "of"]++) (possibleAliases' elemsType)
            in if elemsType == CharT then ["strings"]:aliases else aliases
        possibleAliases' t  = [typeName t True]
possibleAliases paramType =  [typeName paramType False]

-- | Turns a number into an ordinal.
intToPosition :: Int -> String
intToPosition 1 = "1st"
intToPosition 2 = "2nd"
intToPosition 3 = "3rd"
intToPosition n
    | n < 10 = show n ++ "th"
    | otherwise = show (div n 10) ++ intToPosition (mod n 10)

-- | Returns a list with the type of each parameter in a title.
titleTypes :: Title a -> [Type]
titleTypes [] = []
titleTypes (TitleWords _ _ : parts) = titleTypes parts
titleTypes (TitleParam _ _ paramType : parts) = paramType : titleTypes parts

-- | Returns a list with the names of each parameter in a title.
parameterNames :: Title a -> [Name]
parameterNames [] = []
parameterNames (TitleWords _ _ : parts) = parameterNames parts
parameterNames (TitleParam _ names _ : parts) = names ++ parameterNames parts

-- | Returns a list of aliases for each parameter in a title.
computeAliases :: Title a -> [[Name]]
computeAliases parts =
    let types = titleTypes parts
        paramNames = parameterNames parts
        possAs = map possibleAliases types
        (_, totAs) = runState (countRepetitions (concat possAs) >> cancelNames paramNames) M.empty
        (as, _) = runState (mapM (`getParameterAliases` totAs) possAs) M.empty
    in as
    where
        -- A computation that accumulates how many times each name in a list appears in it.
        countRepetitions :: [Name] -> State (M.Map Name Int) ()
        countRepetitions = mapM_ (\n -> modify $ M.insertWith (+) n 1)

        -- A computation that sets to -1 the counts of the names in a list.
        cancelNames :: [Name] -> State (M.Map Name Int) ()
        cancelNames = mapM_ $ \(w:ws) -> do
            modify $ M.insert (w:ws) (-1)
            unless (w == "the") $ modify (M.insert ("the":w:ws) 0)

        -- A computation that takes the aliases of a parameter and adds the appropriate ordinals to them, removing from the list the ones that are already in use.
        getParameterAliases :: [Name] -> M.Map Name Int -> State (M.Map Name Int) [Name]
        getParameterAliases names totAs = do
            countRepetitions names
            ordNames <- mapM (`addOrdinal` totAs) names
            return $ filter (\name -> not $ M.member name totAs) ordNames

        -- A computation that adds an ordinal to an alias depending on the number of its current apparition and the number of times it appears in total.
        addOrdinal :: Name -> M.Map Name Int -> State (M.Map Name Int) Name
        addOrdinal name totAs = do
            let totReps = totAs M.! name
            if totReps > 1
                then do
                    currRep <- gets (M.! name)
                    return $ "the" : intToPosition currRep : name
                else return $ "the" : name

-- | Adds the possible aliases to each parameter in a title.
addAliases :: Title a -> Title a
addAliases parts = addAliases' parts $ computeAliases parts
    where
        addAliases' :: Title a -> [[Name]] -> Title a
        addAliases' parts [] = parts
        addAliases' (TitleWords ann ws : parts) as = TitleWords ann ws : addAliases' parts as
        addAliases' (TitleParam ann [] pType : parts) (as:rest) = TitleParam ann as pType : addAliases' parts rest
        addAliases' (TitleParam ann ns pType : parts) (_:rest) = TitleParam ann ns pType : addAliases' parts rest
        addAliases' [] (_:_) = error "Shouldn't happen: can't run out of title parts before running out of types"


-- -----------------
-- * Types information

-- | Whether the given type satisfies the expected type.
satisfiesType ::
    Type -- ^ The type of a value.
    -> Type -- ^ The type that should be satisfied.
    -> Bool
satisfiesType _ (AnyT _) = True
satisfiesType IntT FloatT = True
satisfiesType (ListT t1) (ListT t2) = t1 `satisfiesType` t2
satisfiesType (RefT t1) (RefT t2) = t1 `satisfiesType` t2
satisfiesType (RefT t1) t2 = t1 `satisfiesType` t2
satisfiesType t1 t2 = t1 == t2

-- | Whether the given type is considered a primitive (including strings).
isPrimitiveType :: Type -> Bool
isPrimitiveType (ListT eT) = eT == CharT
isPrimitiveType (AnyT _) = False
isPrimitiveType (RefT _) = False
isPrimitiveType _ = True

getValueType :: Annotated Value -> SolverEnv Type
getValueType (IntV _ _) = return IntT
getValueType (FloatV _ _) = return FloatT
getValueType (BoolV _ _) = return BoolT
getValueType (CharV _ _) = return CharT
getValueType (ListV _ t _) = return $ ListT t
getValueType (VarV _ n) = RefT <$> getVariableType n
getValueType (OperatorCall _ fid args) = do
    ~(FunSignature _ (Operator typeFun)) <- getFunctionSignature fid
    argTypes <- getArgumentTypesWithCheck (fid, args)
    return $ typeFun argTypes
getValueType (IterV _ iterType listVal) = do
    listType <- withLocation listVal getValueType
    case iterType of
        Just elemsType -> do
            let expType = ListT elemsType
            if listType `satisfiesType` expType
                then return $ RefT (getElementsType listType)
                else throwHere $ WrongTypeValue expType listType
        Nothing -> return $ RefT (getElementsType listType)
getValueType (InputV _ expType) = return expType
getValueType (ValueM _ _) = error "Shouldn't happen: values must be solved before getting their types"
getValueType (RefV _ _) = error "Shouldn't happen: references can't exist before evaluating"

getElementsType :: Type -> Type
getElementsType (RefT refType) = getElementsType refType
getElementsType (ListT elemsType) = elemsType
getElementsType _ = error "Shouldn't happen: type doesn't contain types"


-- -----------------
-- * Validations

-- | Sets the type of a variable, failing if it was already defined.
setNewVariableType :: Name -> Type -> SolverEnv ()
setNewVariableType name varType = do
    isDef <- variableIsDefined name
    when isDef $ throwHere (VariableAlreadyDefined name)
    setVariableType name varType

-- | Fails if the type of a value doesn't satisfy the given type.
checkValueType :: Annotated Value -> Type -> SolverEnv ()
checkValueType val expType = do
    valType <- withLocation val getValueType
    unless (valType `satisfiesType` expType) $ throwHere (WrongTypeValue expType valType)

-- | Validates the types of the arguments in a procedure call.
checkProcedureCallType :: Annotated Sentence -> SolverEnv ()
checkProcedureCallType (ProcedureCall ann fid args) = void $ getArgumentTypesWithCheck (fid, args)
checkProcedureCallType _ = error "Shouldn't happen: sentence given is not a procedure call"

-- | Returns the types of the arguments in a function call.
-- Validates that their types are correct and transforms them accordingly, taking type bindings into account.
getArgumentTypesWithCheck :: (FunId, [Annotated Value]) -> SolverEnv [Type]
getArgumentTypesWithCheck (fid, args) = do
    (FunSignature title _) <- getFunctionSignature fid
    getArgumentTypesWithCheck' M.empty title args 0
    where
        -- Returns the type of all arguments given a mapping of type bindings.
        getArgumentTypesWithCheck' :: M.Map String Type -> Bare Title -> [Annotated Value] -> Int -> SolverEnv [Type]
        getArgumentTypesWithCheck' _ _ [] _ = return []
        getArgumentTypesWithCheck' bindings (TitleWords {} : titleParts) args paramNum = getArgumentTypesWithCheck' bindings titleParts args paramNum
        getArgumentTypesWithCheck' bindings (TitleParam _ _ paramType : titleParts) (arg:args) paramNum = do
            ann <- getCurrentLocation
            argType <- withLocation arg getValueType
            unless (argType `satisfiesType` paramType) $ throwHere (WrongTypeArgument paramType argType paramNum fid)
            -- Shift the argument's type to the reference level expected by the parameter.
            let argType' = solveTypeReferences paramType argType
            argsTypes <- case getTypeBinding paramType argType' of
                Just ((typeId, boundType), typeFun) ->
                    case M.lookup typeId bindings of
                        -- If the type being bound was already bound by a previous parameter, check that both types match.
                        Just expBoundType -> do
                            unless (boundType `satisfiesType` expBoundType) $ throwHere (WrongTypeArgument (typeFun expBoundType) argType' paramNum fid)
                            getArgumentTypesWithCheck' bindings titleParts args (paramNum+1)
                        -- If a new type is being bound, add it to the known bindings.
                        Nothing -> getArgumentTypesWithCheck' (M.insert typeId boundType bindings) titleParts args (paramNum+1)
                Nothing -> getArgumentTypesWithCheck' bindings titleParts args (paramNum+1)
            setCurrentLocation ann
            return (argType':argsTypes)
        getArgumentTypesWithCheck' _ [] (_:_) _ = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

        -- Returns the type bound by an argument in a parameter, and a function to build the whole type when given the bound type.
        -- This works because a parameter's type can contain only one type binding.
        getTypeBinding ::
            Type -- The type of the parameter, which may contain a bindable type.
            -> Type -- The type of the argument, which may assign a specific type to a bindable type.
            -> Maybe ((String, Type), Type -> Type)
        getTypeBinding (ListT paramElemsType) (ListT argElemsType) = do
            -- Find bindings in the types of the elements of the lists.
            (typeBinding, typeFun) <- getTypeBinding paramElemsType argElemsType
            return (typeBinding, ListT . typeFun)
        getTypeBinding (RefT paramRefType) (RefT argRefType) = do
            -- Find bindings in the referenced types.
            (typeBinding, typeFun) <- getTypeBinding paramRefType argRefType
            return (typeBinding, RefT . typeFun)
        getTypeBinding (AnyT typeId) argType = Just ((typeId, argType), id) -- Type binding found.
        getTypeBinding _ _ = Nothing -- The type contains no bindable types.

        -- Takes the type of an argument and the type of a parameter, and returns the argument's type in the appropriate reference level.
        -- This works properly only if the argument's type satisfies the parameter's type.
        solveTypeReferences ::
            Type -- The parameter's type.
            -> Type -- The argument's type
            -> Type
        solveTypeReferences (RefT paramRefType) (RefT argRefType) = RefT $ solveTypeReferences paramRefType argRefType
        solveTypeReferences paramType (RefT argRefType) = solveTypeReferences paramType argRefType
        solveTypeReferences (ListT paramElemsType) (ListT argElemsType) = ListT $ solveTypeReferences paramElemsType argElemsType
        solveTypeReferences _ argType = argType

-- | Fails if the given value contains any 'IterV'.
checkNoIterators :: Annotated Value -> SolverEnv ()
checkNoIterators val = when (hasIterators val) $ throwHere ForbiddenIteratorUsed

-- | Searches for `break` statements in the given sentence and fails if it is not inside a loop.
checkBreaks :: Annotated Sentence -> SolverEnv ()
checkBreaks (When _ _ ss) = mapM_ (`withLocation` checkBreaks) ss
checkBreaks (Unless _ _ ss) = mapM_ (`withLocation` checkBreaks) ss
checkBreaks (IfElse  _ _ ssTrue ssFalse) = mapM_ (`withLocation` checkBreaks) $ ssTrue ++ ssFalse
checkBreaks (Attempt _ ss) = mapM_ (`withLocation` checkBreaks) ss
checkBreaks (TryCatch _ ssTry ssCatch) = mapM_ (`withLocation` checkBreaks) $ ssTry ++ ssCatch
checkBreaks (ForEach {}) = return ()
checkBreaks (While {}) = return ()
checkBreaks (Until {}) = return ()
checkBreaks (Break _) = throwHere BreakOutsideLoop
checkBreaks _ = return ()


-- -----------------
-- * Registration

-- | Modifies a program, adding autogenerated aliases to unnamed function parameters.
registerAliases :: Program -> SolverEnv Program
registerAliases = return . map (\(FunDef ann title retType ss) -> FunDef ann (addAliases title) retType ss)

-- | Adds all the user-defined functions to the state.
registerFunctions :: Program -> SolverEnv ()
registerFunctions = mapM_ (`withLocation` registerFunction)
    where
        registerFunction :: Annotated Definition -> SolverEnv ()
        registerFunction (FunDef _ title retType _) = do
            let fid = getFunId title
                funRetType = maybe Procedure (Operator . const) retType
            isDef <- functionIsDefined fid
            when isDef $ throwHere (FunctionAlreadyDefined fid)
            setFunctionSignature fid $ FunSignature (map void title) funRetType

-- | Adds the parameters of a function to the state.
registerParameters :: Annotated Title -> SolverEnv ()
registerParameters = mapM_ (`withLocation` registerParameter)
    where
        registerParameter :: Annotated TitlePart -> SolverEnv ()
        registerParameter (TitleParam ann names paramType) = mapM_ (`setNewVariableType` paramType) names
        registerParameter _ = return ()


-- -----------------
-- * Solvers

-- | Chooses a way to understand a possibly ambiguous value.
solveValue :: (Annotated Value -> SolverEnv ()) -> Annotated Value -> SolverEnv (Annotated Value)
solveValue validate (ValueM _ parts) = do
    res <- matchAsValue parts
    case res of
        [] -> throwHere $ UnmatchableValue parts
        [val] -> validate val >> return val
        vals -> do
            let tryValue val = (validate val >> return (Just val)) `catchError` (\_ -> return Nothing)
            validVals <- catMaybes <$> mapM tryValue vals
            case validVals of
                [val] -> return val
                [] -> throwHere $ UnmatchableValueTypes parts
                _ -> do
                    -- If there are many valid ways to understand a value, return the first one and issue a warning.
                    warnHere $ AmbiguousValue (length validVals) parts
                    return $ head validVals
solveValue validate val@(ListV ann elemsType elems) = do
    validate val
    elems' <- mapM (`withLocation` solveValueWithType elemsType True) elems
    return $ ListV ann elemsType elems'
solveValue validate val@(InputV _ expType) = do
    validate val
    -- Don't allow complex types.
    unless (isPrimitiveType expType) $ throwHere (UnreadableType expType)
    return val
solveValue validate val = validate val >> return val

-- | Solves a value and validates that it satisfies a type.
-- If the boolean is false, also validates that the value contains no iterators.
solveValueWithType ::
    Type -- ^ The type that the value should satisfy.
    -> Bool -- ^ Whether iterators should be allowed.
    -> Annotated Value -- ^ The value to solve and validate.
    -> SolverEnv (Annotated Value)
solveValueWithType expType allowIters val = do
    val' <- solveValue (`checkValueType` expType) val
    unless allowIters $ withLocation val' checkNoIterators
    return val'

-- | Solves a value and validates that it contains no iterators.
solveValueWithAnyType :: Annotated Value -> SolverEnv (Annotated Value)
solveValueWithAnyType val = do
    ann <- getCurrentLocation
    val' <- solveValue (void . getValueType) val
    withLocation val' checkNoIterators
    return val'

-- | Chooses a way to understand a possibly ambiguous sentence.
solveSentence :: Maybe Type -> Annotated Sentence -> SolverEnv (Annotated Sentence)
solveSentence _ (VarDef ann names (Just expType) val) = do
    val' <- case val of
        (ListV {}) -> withLocation val (solveValueWithType expType True)
        _ -> withLocation val (solveValueWithType expType False)
    setCurrentLocation ann
    mapM_ (`setNewVariableType` expType) names
    return $ VarDef ann names (Just expType) val'
solveSentence _ (VarDef ann names Nothing val) = do
    val' <- withLocation val solveValueWithAnyType
    valType <- getValueType val'
    setCurrentLocation ann
    mapM_ (`setNewVariableType` valType) names
    return $ VarDef ann names Nothing val'

solveSentence retType (When ann cond ss) = do
    cond' <- withLocation cond $ solveValueWithType BoolT False
    ss' <- solveSentences ss retType
    return $ When ann cond' ss'
solveSentence retType (Unless ann cond ss) = do
    cond' <- withLocation cond $ solveValueWithType BoolT False
    ss' <- solveSentences ss retType
    return $ Unless ann cond' ss'
solveSentence retType (IfElse ann cond ssTrue ssFalse) = do
    cond' <- withLocation cond $ solveValueWithType BoolT False
    ssTrue' <- solveSentences ssTrue retType
    ssFalse' <- solveSentences ssFalse retType
    return $ IfElse ann cond' ssTrue' ssFalse'

solveSentence retType (ForEach ann iterNames iterType listVal ss) = do
    listVal' <- withLocation listVal $ solveValueWithType (ListT iterType) False
    setCurrentLocation ann
    -- Use the type as iterator name if no names where provided.
    let iterNames' = if null iterNames then [typeName iterType False] else iterNames
    mapM_ (`setNewVariableType` RefT iterType) iterNames'
    ss' <- solveSentences ss retType
    mapM_ removeVariableType iterNames'
    return $ ForEach ann iterNames' iterType listVal' ss'

solveSentence retType (Until ann cond ss) = do
    cond' <- withLocation cond $ solveValueWithType BoolT False
    ss' <- solveSentences ss retType
    return $ Until ann cond' ss'
solveSentence retType (While ann cond ss) = do
    cond' <- withLocation cond $ solveValueWithType BoolT False
    ss' <- solveSentences ss retType
    return $ While ann cond' ss'

solveSentence retType (Return ann val) =
    case retType of
        Just expType -> do
            val' <- withLocation val $ solveValueWithType expType False
            return $ Return ann val'
        Nothing -> throwHere ResultInProcedure
solveSentence retType s@(Break _) = return s
solveSentence retType s@(Exit _) =
    case retType of
        Just _ -> throwHere ExitInOperator
        Nothing -> return s

solveSentence retType (Attempt ann ss) = Attempt ann <$> solveSentences ss retType
solveSentence retType (TryCatch ann ssTry ssCatch) = do
    ssTry' <- solveSentences ssTry retType
    ssCatch' <- solveSentences ssCatch retType
    return $ TryCatch ann ssTry' ssCatch'
solveSentence retType (Throw ann msg) = return $ Throw ann msg

solveSentence _ (Read ann valType val) = do
    -- Don't allow complex types.
    unless (isPrimitiveType valType) $ throwHere (UnreadableType valType)
    val' <- withLocation val (solveValueWithType (RefT valType) False)
    return $ Read ann valType val'
solveSentence _ (SentenceM _ parts) = do
    res <- matchAsSentence parts
    case res of
        [] -> throwHere $ UnmatchableSentence parts
        [s] -> checkProcedureCallType s >> return s
        ss -> do
            let trySentence s = (checkProcedureCallType s >> return (Just s)) `catchError` (\_ -> return Nothing)
            validSs <- catMaybes <$> mapM trySentence ss
            case validSs of
                [s] -> return s
                [] -> throwHere $ UnmatchableSentenceTypes parts
                _ -> do
                    -- If there are many valid ways to understand a sentence, return the first one and issue a warning.
                    warnHere $ AmbiguousSentence (length validSs) parts
                    return $ head validSs
solveSentence _ (ProcedureCall {}) = error "Shouldn't happen: procedure calls can only be created by solving a matchable"

-- | Solves a list of sentences in a new block scope, discarding it afterwards.
solveSentences :: [Annotated Sentence] -> Maybe Type -> SolverEnv [Annotated Sentence]
solveSentences ss retType = restoringVariables $ mapM (`withLocation` solveSentence retType) ss

solveDefinition :: Annotated Definition -> SolverEnv (Annotated Definition)
solveDefinition (FunDef ann title retType ss) = do
    -- Register the parameters in the title of the function and solve the sentences in its body.
    setCurrentLocation $ getFirstLocation title
    registerParameters title
    mapM_ (`withLocation` checkBreaks) ss
    ss' <- solveSentences ss retType
    return $ FunDef ann title retType ss'


-- -----------------
-- * Main

-- | Returns the result of solving a whole program, making all integrity validations.
solveProgram :: Program -> (Either Error ((Program, Location), SolverData), [Warning])
solveProgram prog = runSolverEnv (solveProgram' prog) [] initialLocation initialState
    where
        solveProgram' :: Program -> SolverEnv Program
        solveProgram' prog = do
            -- Add aliases to the parameters of user-defined functions.
            prog' <- registerAliases prog
            -- Register user-defined functions in the state.
            registerFunctions prog'
            -- Check that the "main" function is defined.
            mainIsDef <- functionIsDefined "run"
            unless mainIsDef $ throwNowhere (UndefinedFunction "run")
            -- Solve user-defined functions, cleaning the variables from the state after each one.
            mapM (restoringVariables . solveDefinition) prog'
