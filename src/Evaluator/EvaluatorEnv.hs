{-|
Module      : EvaluatorEnv
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

The monad on which the "Evaluator" runs, with some useful operations.
-}

module EvaluatorEnv (module EvaluatorEnv, module Location) where

import Control.Monad (when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets, modify, runStateT, StateT)
import Control.Monad.Trans.Except (runExceptT, ExceptT)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M

import AST
import BuiltInDefs (builtInFunctions)
import Errors
import Location


-- -----------------
-- * Type definitions

-- | The evaluator has a pointer to the next empty address to use when storing a new value.
type Pointer = Int

-- | The memory threshold at which the garbage collector is triggered.
type Memory = Int

-- | A mapping between function ids and their callables.
type FunData = M.Map FunId FunCallable

-- | One mapping between variable names and their addresses for each scope.
type VarData = [M.Map Name Int]

-- | A mapping between addresses and their values.
type RefData = IM.IntMap (Bare Value)

-- | The state of the evaluator.
type EvaluatorData = (FunData, VarData, RefData, Pointer, Memory)

-- | The evaluator's monad.
-- It stores the evaluator's state.
-- It has the ability to throw and catch errors.
-- It stores the current location.
-- It is parametrized over an inner monad m, which is used input and output in a testable way.
type EvaluatorEnv m a = LocationT (StateT EvaluatorData (ExceptT Error m)) a

runEvaluatorEnv :: EvaluatorEnv m a -> Location -> EvaluatorData -> m (Either Error ((a, Location), EvaluatorData))
runEvaluatorEnv action location state = runExceptT $ runStateT (runLocationT action location) state

-- | The default initial state of the evaluator, including an empty variables scope and the built-in functions.
initialState :: EvaluatorData
initialState =
    let functions = M.fromList $ map (\(funId, FunSignature title _) -> (funId, FunCallable title [])) builtInFunctions
    in (functions, [M.empty], IM.empty, 0, initMaxMem)
    where initMaxMem = 4

-- | A class of monad which allows to read from an input and write to an output.
class Monad m => ReadWrite m where
    read :: m String
    write :: String -> m ()

-- | The IO monad is an instance of 'ReadWrite', since it can read from stdin and write to stdout.
instance ReadWrite IO where
    read = getLine
    write = putStr

-- | Lift a computation in the inner monad.
liftReadWrite :: ReadWrite m => m a -> EvaluatorEnv m a
liftReadWrite = lift . lift . lift


-- -----------------
-- * Errors

-- | Catches an 'Error' but only if it is a 'CodeError', which can be recovered from.
catchCodeError :: Monad m => EvaluatorEnv m a -> (Error -> EvaluatorEnv m a) -> EvaluatorEnv m a
catchCodeError action handler =
    action `catchError` (\err ->
        case err of
            (Error _ (CodeError msg)) -> handler err
            _ -> throwError err)

-- | Throw an error of the given type at the current location.
throwHere :: Monad m => ErrorType -> EvaluatorEnv m a
throwHere errType = do
    location <- getCurrentLocation
    throwError $ Error (Just location) errType


-- -----------------
-- * Operations on functions

-- | Modifies the state through a 'FunData' modifier.
modifyFunctions :: Monad m => (FunData -> FunData) -> EvaluatorEnv m ()
modifyFunctions modF = lift $ modify (\(funs, vars, refs, ptr, mem) -> (modF funs, vars, refs, ptr, mem))

getFunctions :: Monad m => EvaluatorEnv m FunData
getFunctions = lift $ gets (\(funs, vars, refs, ptr, mem) -> funs)

-- | Returns the 'FunCallable' of a function, assuming it is defined.
getFunctionCallable :: Monad m => FunId -> EvaluatorEnv m FunCallable
getFunctionCallable fid = (M.! fid) <$> getFunctions

setFunctionCallable :: Monad m => FunId -> FunCallable -> EvaluatorEnv m ()
setFunctionCallable fid funC = modifyFunctions $ M.insert fid funC


-- -----------------
-- * Operations on references

-- | Modifies the state through a 'RefData' modifier.
modifyReferences :: Monad m => (RefData -> RefData) -> EvaluatorEnv m ()
modifyReferences modF = lift $ modify (\(funs, vars, refs, ptr, mem) -> (funs, vars, modF refs, ptr, mem))

getReferences :: Monad m => EvaluatorEnv m RefData
getReferences = lift $ gets (\(funs, vars, refs, ptr, mem) -> refs)

-- | Returns the value at an address, assuming it is allocated.
getValueAtAddress :: Monad m => Int -> EvaluatorEnv m (Bare Value)
getValueAtAddress addr = (IM.! addr) <$> getReferences

setValueAtAddress :: Monad m => Int -> Bare Value -> EvaluatorEnv m ()
setValueAtAddress addr v = modifyReferences $ IM.insert addr v

-- | Replaces the references in a value with the values pointed at by them.
loadReferences :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
loadReferences (ListV _ elemsType refVals) = ListV () elemsType <$> mapM loadReferences refVals
loadReferences (RefV _ addr) = getValueAtAddress addr >>= loadReferences
loadReferences val = return val

-- | Stores a new value at the next empty position and returns its address.
addValue :: Monad m => Bare Value -> EvaluatorEnv m Int
addValue val = do
    ptr <- getPointer
    setValueAtAddress ptr val
    modifyPointer (+1)
    return ptr

-- | Returns a deep copy of the given value.
copyValue :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
copyValue (ListV _ elemsType vals) = ListV () elemsType <$> mapM copyValue vals
copyValue (RefV _ addr) = do
    val <- getValueAtAddress addr
    val' <- copyValue val
    RefV () <$> addValue val'
copyValue val = return val


-- -----------------
-- * Operations on variables

-- | Modifies the state through a 'VarData' modifier.
modifyVariables :: Monad m => (VarData -> VarData) -> EvaluatorEnv m ()
modifyVariables modF = lift $ modify (\(funs, vars, refs, ptr, mem) -> (funs, modF vars, refs, ptr, mem))

getVariables :: Monad m => EvaluatorEnv m VarData
getVariables = lift $ gets (\(funs, vars, refs, ptr, mem) -> vars)

-- | Whether a variable is defined in the current scope.
variableIsDefined :: Monad m => Name -> EvaluatorEnv m Bool
variableIsDefined name = M.member name . head <$> getVariables

-- | Returns the address of a variable, assuming it is defined.
getVariableAddress :: Monad m => Name -> EvaluatorEnv m Int
getVariableAddress name = lift $ gets (\(_, vs:_, _, _, _) -> vs M.! name)

setVariableAddress :: Monad m => Name -> Int -> EvaluatorEnv m ()
setVariableAddress name addr = modifyVariables (\(scope:scopes) -> M.insert name addr scope : scopes)

removeVariable :: Monad m => Name -> EvaluatorEnv m ()
removeVariable name = modifyVariables (\(scope:scopes) -> M.delete name scope : scopes)

-- | Returns the value of a variable, assuming it is defined.
getVariableValue :: Monad m => Name -> EvaluatorEnv m (Bare Value)
getVariableValue name = getVariableAddress name >>= getValueAtAddress

-- | Sets the value of a variable.
-- If the variable is not defined, the value is stored at the next empty address.
-- If the variable is defined, the value is stored at its address, overwriting the previous value.
setVariableValue :: Monad m => Name -> Bare Value -> EvaluatorEnv m ()
setVariableValue name val = do
    isDef <- variableIsDefined name
    if isDef
        then getVariableAddress name >>= (`setValueAtAddress` val)
        else addValue val >>= setVariableAddress name

-- | Receives a list of new variables to be declared and a list of references to be set and runs a computation with those variables in scope.
-- New variables are discarded after the action.
-- If a value is passed by reference, it can be modified inside the computation.
inNewScope :: Monad m => EvaluatorEnv m a -> [([Name], Bare Value)] -> [([Name], Int)] -> EvaluatorEnv m a
inNewScope action newVarVals newVarRefs = do
    -- Create a new empty scope.
    modifyVariables (M.empty:)
    -- For each value passed by copy, store it in memory and point the corresponding variables to its new address.
    mapM_ (\(name:names, val) -> do
        setVariableValue name val
        addr <- getVariableAddress name
        -- Point the aliases of a variable to its address.
        mapM_ (`setVariableAddress` addr) names
        ) newVarVals
    -- For each value passed by reference, point the corresponding variables to its address.
    mapM_ (\(names, addr) -> mapM_ (`setVariableAddress` addr) names) newVarRefs
    -- Runs the computation using only the given variables.
    result <- action
    -- Discard the new scope and return the result.
    modifyVariables tail
    return result

-- | Run a computation in a block scope, discarding variables created inside it after it ends.
inBlockScope :: Monad m => EvaluatorEnv m a -> EvaluatorEnv m a
inBlockScope action = do
    varsSet <- M.keysSet . head <$> getVariables
    result <- action
    modifyVariables (\(scope:scopes) -> M.restrictKeys scope varsSet : scopes)
    return result


-- -----------------
-- * Operations on the pointer

-- | Modifies the state through a 'Pointer' modifier.
modifyPointer :: Monad m => (Pointer -> Pointer) -> EvaluatorEnv m ()
modifyPointer modF = lift $ modify (\(funs, vars, refs, ptr, mem) -> (funs, vars, refs, modF ptr, mem))

getPointer :: Monad m => EvaluatorEnv m Pointer
getPointer = lift $ gets (\(funs, vars, refs, ptr, mem) -> ptr)


-- -----------------
-- * Operations on memory

-- | Modifies the state through a 'Memory' modifier.
modifyMaxMemory :: Monad m => (Memory -> Memory) -> EvaluatorEnv m ()
modifyMaxMemory modF = lift $ modify (\(funs, vars, refs, ptr, mem) -> (funs, vars, refs, ptr, modF mem))

getMaxMemory :: Monad m => EvaluatorEnv m Memory
getMaxMemory = lift $ gets (\(funs, vars, refs, ptr, mem) -> mem)

getUsedMemory :: Monad m => EvaluatorEnv m Int
getUsedMemory = lift $ gets (\(_, _, rs, _, _) -> IM.size rs)


-- -----------------
-- * Garbage collector

-- | Starts the garbage collector if the used memory is larger than the max memory threshold.
tick :: Monad m => EvaluatorEnv m ()
tick = do
    usedMem <- getUsedMemory
    maxMem <- getMaxMemory
    when (usedMem >= maxMem) collectGarbage

-- | Frees the addresses that are considered unreachable.
collectGarbage :: Monad m => EvaluatorEnv m ()
collectGarbage = do
    rAddrs <- getReachableAddresses
    let keysSet = IS.fromList rAddrs
    modifyReferences (`IM.restrictKeys` keysSet)
    usedMem <- getUsedMemory
    modifyMaxMemory $ const (usedMem * 2)

-- | Returns the addresses that are reachable from the variable definitions in any of the scopes.
getReachableAddresses :: Monad m => EvaluatorEnv m [Int]
getReachableAddresses = do
    scopes <- getVariables
    let varAddrs = concatMap M.elems scopes
    varVals <- mapM getValueAtAddress varAddrs
    valsAddrs <- concat <$> mapM getReachableFromValue varVals
    return (varAddrs ++ valsAddrs)
    where
        getReachableFromValue :: Monad m => Bare Value -> EvaluatorEnv m [Int]
        getReachableFromValue (RefV _ addr) = do
            val <- getValueAtAddress addr
            (addr:) <$> getReachableFromValue val
        getReachableFromValue (ListV _ _ refVals) = concat <$> mapM getReachableFromValue refVals
        getReachableFromValue _ = return []
