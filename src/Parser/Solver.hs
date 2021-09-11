{-|
Module      : Solver
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The language's second phase of parsing.
Transforms the parts left unparsed by the "FuzzyParser" into concrete Values or Sentences.
-}

module Solver where

import Control.Monad (unless, when, void)
import Control.Monad.Trans.State (gets, modify, runState, State)
import Data.Maybe (fromJust, catMaybes)
import qualified Data.Map.Strict as M

import AST
import Errors
import Matchers
import SolverEnv
import Utils (hasIterators, typeName, getFunId)


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
titleTypes :: [TitlePart a] -> [Type]
titleTypes [] = []
titleTypes (TitleWords _ _ : parts) = titleTypes parts
titleTypes (TitleParam _ _ paramType : parts) = paramType : titleTypes parts

-- | Returns a list with the names of each parameter in a title.
parameterNames :: [TitlePart a] -> [Name]
parameterNames [] = []
parameterNames (TitleWords _ _ : parts) = parameterNames parts
parameterNames (TitleParam _ names _ : parts) = names ++ parameterNames parts

-- | Returns a list of aliases for each parameter in a title.
computeAliases :: [TitlePart a] -> [[Name]]
computeAliases parts =
    let types = titleTypes parts
        paramNames = parameterNames parts
        possAs = map possibleAliases types
        (_, totAs) = runState (countRepetitions (concat possAs) >> cancelNames paramNames) M.empty
        (as, _) = runState (mapM (`getParameterAliases` totAs) possAs) M.empty
    in as
    where
        -- A computation that counts how many times each name in a list appears in it.
        countRepetitions :: [Name] -> State (M.Map Name Int) ()
        countRepetitions = mapM_ (\n -> modify $ M.insertWith (+) n 1)

        -- A computation that sets to 0 the counts of the names in a list.
        -- ToDo: this is not needed if variables cant be named the same as a possible alias.
        cancelNames :: [Name] -> State (M.Map Name Int) ()
        cancelNames = mapM_ $ \(w:ws) -> do
            modify $ M.insert (w:ws) 0
            unless (w == "the") $ modify (M.insert ("the":w:ws) 0)

        -- A computation that takes the aliases of a parameter and adds the appropriate ordinals to them, removing from the list the ones that are already in use.
        -- ToDo: this is not needed if variables cant be named the same as a possible alias.
        getParameterAliases :: [Name] -> M.Map Name Int -> State (M.Map Name Int) [Name]
        getParameterAliases names totAs = do
            countRepetitions names
            ordNames <- mapM (`addOrdinal` totAs) names
            return $ filter (\name -> not $ M.member name totAs) ordNames

        -- A computation that adds an ordinal to an alias depending on the number of its current aparition and the number of times it appears in total.
        addOrdinal :: Name -> M.Map Name Int -> State (M.Map Name Int) Name
        addOrdinal name totAs = do
            let totReps = totAs M.! name
            if totReps > 1
                then do
                    currRep <- gets (M.! name)
                    return $ "the" : intToPosition currRep : name
                else return $ "the" : name

-- | Adds the possible aliases to each parameter in a title.
addAliases :: [TitlePart a] -> [TitlePart a]
addAliases parts = addAliases' parts $ computeAliases parts
    where
        addAliases' :: [TitlePart a] -> [[Name]] -> [TitlePart a]
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

getValueType :: Annotated Value -> SolverEnv Type
getValueType (IntV _ _) = return IntT
getValueType (FloatV _ _) = return FloatT
getValueType (BoolV _ _) = return BoolT
getValueType (CharV _ _) = return CharT
getValueType (ListV _ t _) = return $ ListT t
getValueType (VarV _ n) = RefT . fromJust <$> getVariableType n
getValueType (OperatorCall _ fid args) = do
    ~(FunSignature _ (Operator typeFun)) <- fromJust <$> getFunctionSignature fid
    argTypes <- getArgumentTypesWithCheck (fid, args)
    return $ typeFun argTypes
getValueType (IterV _ iterType listVal) = do
    listType <- withLocation listVal getValueType
    let expType = ListT iterType
    if listType `satisfiesType` expType
        then return $ RefT (getElementsType listType)
        else throwHere $ WrongTypeValue expType listType
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
    ~(FunSignature (Title _ funTitle) _) <- fromJust <$> getFunctionSignature fid
    getArgumentTypesWithCheck' M.empty funTitle args 0
    where
        -- Returns the type of all arguments given a mapping of type bindings.
        getArgumentTypesWithCheck' :: M.Map String Type -> [Bare TitlePart] -> [Annotated Value] -> Int -> SolverEnv [Type]
        getArgumentTypesWithCheck' _ _ [] _ = return []
        getArgumentTypesWithCheck' bindings (TitleWords {} : titleParts) args paramNum = getArgumentTypesWithCheck' bindings titleParts args paramNum
        getArgumentTypesWithCheck' bindings (TitleParam _ _ paramType : titleParts) (arg:args) paramNum = do
            ann <- getCurrentLocation
            argType <- withLocation arg getValueType
            unless (argType `satisfiesType` paramType) $ throwHere (WrongTypeArgument paramType argType paramNum fid)
            -- Shift the argument's type to the reference level expected by the parameter.
            let argType' = solveTypeReferences paramType argType
            argTypes <- case getTypeBinding paramType argType' of
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
            return (argType':argTypes)
        getArgumentTypesWithCheck' _ [] (_:_) _ = error "Shouldn't happen: can't run out of title parts before running out of values in a function call"

        -- Returns the type bound by the given parameter and argument, and a function to build the whole type when given the bound type.
        getTypeBinding :: Type -> Type -> Maybe ((String, Type), Type -> Type)
        getTypeBinding (ListT paramElemsType) (ListT argElemsType) = do
            -- Find bindings in the types of the elements of the lists.
            (typeBinding, typeFun) <- getTypeBinding paramElemsType argElemsType
            return (typeBinding, ListT . typeFun)
        getTypeBinding (RefT paramRefType) (RefT argRefType) = do
            -- Find bindings in the referenced types.
            (typeBinding, typeFun) <- getTypeBinding paramRefType argRefType
            return (typeBinding, RefT . typeFun)
        getTypeBinding (AnyT typeId) argType = Just ((typeId, argType), id) -- Type binding found.
        getTypeBinding _ _ = Nothing

        -- Takes the type of an argument and the type of a parameter, and returns the argument's type in the appropriate reference level.
        -- This works properly only if the argument's type satisfies the parameter's type.
        solveTypeReferences ::
            Type -- ^ The parameter's type.
            -> Type -- ^ The argument's type
            -> Type
        solveTypeReferences (RefT paramRefType) (RefT argRefType) = RefT $ solveTypeReferences paramRefType argRefType
        solveTypeReferences paramType (RefT argRefType) = solveTypeReferences paramType argRefType
        solveTypeReferences (ListT paramElemsType) (ListT argElemsType) = ListT $ solveTypeReferences paramElemsType argElemsType
        solveTypeReferences _ argType = argType

-- | Fails if the given value contains any 'IterV'.
checkNoIterators :: Annotated Value -> SolverEnv ()
checkNoIterators val = when (hasIterators val) $ throwHere ForbiddenIteratorUsed


-- -----------------
-- * Registration

-- | Modifies a program, adding autogenerated aliases to unnamed function parameters.
registerAliases :: Program -> SolverEnv Program
registerAliases = return . map (\(FunDef ann (Title ann' titleParts) retType ss) -> FunDef ann (Title ann' $ addAliases titleParts) retType ss)

-- | Adds all the user-defined functions to the state.
registerFunctions :: Program -> SolverEnv ()
registerFunctions = mapM_ (`withLocation` registerFunction)
    where
        registerFunction :: Annotated Block -> SolverEnv ()
        registerFunction (FunDef _ funTitle@(Title _ parts) retType _) = do
            let fid = getFunId parts
            isDef <- functionIsDefined fid
            when isDef $ throwHere (FunctionAlreadyDefined fid)
            let funRetType = maybe Procedure (Operator . const) retType
            setFunctionSignature fid $ FunSignature (void funTitle) funRetType

-- | Adds the parameters of a function to the state.
registerParameters :: Annotated Title -> SolverEnv ()
registerParameters (Title _ parts) = mapM_ (`withLocation` registerParameter) parts
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
solveValue validate val@((ListV ann elemsType elems)) = do
    validate val
    solvedElems <- mapM (`withLocation` solveValueWithType elemsType True) elems
    return $ ListV ann elemsType solvedElems
solveValue validate val = validate val >> return val

-- | Solves a value and validates that it satisfies a type.
-- If the boolean is false, also validates that the value contains no iterators.
solveValueWithType ::
    Type -- ^ The type that the value should satisfy.
    -> Bool -- ^ Whether iterators should be allowed.
    -> Annotated Value -- ^ The value to solve and validate.
    -> SolverEnv (Annotated Value)
solveValueWithType expType allowIters val = do
    solvedVal <- solveValue (`checkValueType` expType) val
    unless allowIters $ withLocation solvedVal checkNoIterators
    return solvedVal

-- Solves a value and validates that it contains no iterators.
solveValueWithAnyType :: Annotated Value -> SolverEnv (Annotated Value)
solveValueWithAnyType val = do
    ann <- getCurrentLocation
    solvedVal <- solveValue (void . getValueType) val
    setCurrentLocation ann
    checkNoIterators solvedVal
    return solvedVal

-- | Chooses a way to understand a possibly ambiguous sentence.
solveSentence :: Maybe Type -> Annotated Sentence -> SolverEnv (Annotated Sentence)
solveSentence _ (VarDef ann names (Just expType) val) = do
    solvedVal <- case val of
        (ListV {}) -> withLocation val (solveValueWithType expType True)
        _ -> withLocation val (solveValueWithType expType False)
    mapM_ (`setNewVariableType` expType) names
    return $ VarDef ann names (Just expType) solvedVal
solveSentence _ (VarDef ann names Nothing val) = do
    solvedVal <- withLocation val solveValueWithAnyType
    valType <- getValueType solvedVal
    mapM_ (`setNewVariableType` valType) names
    return $ VarDef ann names Nothing solvedVal
solveSentence retType (If ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls retType
    return $ If ann v' ls'
solveSentence retType (IfElse ann v lsT lsF) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    lsT' <- solveSentences lsT retType
    lsF' <- solveSentences lsF retType
    return $ IfElse ann v' lsT' lsF'
solveSentence retType (ForEach ann iN iT v ls) = do
    v' <- withLocation v $ solveValueWithType (ListT iT) False
    setNewVariableType iN (RefT iT)
    ls' <- solveSentences ls retType
    removeVariableType iN
    return $ ForEach ann iN iT v' ls'
solveSentence retType (Until ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls retType
    return $ Until ann v' ls'
solveSentence retType (While ann v ls) = do
    v' <- withLocation v $ solveValueWithType BoolT False
    ls' <- solveSentences ls retType
    return $ While ann v' ls'
solveSentence retType (Return ann v) =
    case retType of
        Just t -> do
            v' <- withLocation v $ solveValueWithType t False
            return $ Return ann v'
        Nothing -> throwHere ResultInProcedure
solveSentence retType (Try ann ss) = Try ann <$> mapM (`withLocation` solveSentence retType) ss
solveSentence retType (TryCatch ann ts cs) = do
    ts' <- mapM (`withLocation` solveSentence retType) ts
    cs' <- mapM (`withLocation` solveSentence retType) cs
    return $ TryCatch ann ts' cs'
solveSentence retType (Throw ann msg) = return $ Throw ann msg
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
solveSentences ss retType = restoringVariables $ mapM (`withLocation` solveSentence retType) ss

solveBlock :: Annotated Block -> SolverEnv (Annotated Block)
solveBlock (FunDef ann t retType ss) = do
    withLocation t registerParameters
    FunDef ann t retType <$> solveSentences ss retType

--


-- Main

solveProgram :: Program -> (Either Error ((Program, Location), SolverData), [Warning])
solveProgram p = runSolverEnv (solveProgram' p) [] initialLocation initialState
    where
        solveProgram' :: Program -> SolverEnv Program
        solveProgram' p = do
            p' <- registerAliases p
            registerFunctions p'
            mainIsDef <- functionIsDefined "run"
            unless mainIsDef $ throwNowhere (UndefinedFunction "run")
            mapM (restoringVariables . solveBlock) p'

--
