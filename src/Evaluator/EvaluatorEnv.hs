module EvaluatorEnv ( module EvaluatorEnv, module Location ) where

import Control.Monad ( when )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State.Strict ( gets, modify, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, runExceptT, ExceptT )
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Errors
import Location
import AST

--


-- Type definitions

type FunData = M.Map FunId FunCallable -- A mapping between function ids and their callables
type VarData = [M.Map Name Int] -- One mapping between variable names and their addresses for each scope
type RefData = IM.IntMap (Bare Value) -- A mapping between addresses and their values

type EvaluatorData = (FunData, VarData, RefData, Int, Int)
type EvaluatorEnv m a = LocationT (StateT EvaluatorData (ExceptT Error m)) a

class Monad m => ReadWrite m where
    read :: m String
    write :: String -> m ()
    writeLn :: String -> m ()

instance ReadWrite IO where
    read = getLine
    write = putStr
    writeLn = putStrLn

runEvaluatorEnv :: EvaluatorEnv m a -> EvaluatorData -> Location -> m (Either Error ((a, Location), EvaluatorData))
runEvaluatorEnv f d s = runExceptT $ runStateT (runLocationT f s) d

initialState :: EvaluatorData
initialState = (M.empty, [], IM.empty, 0, initMaxMem)
    where initMaxMem = 4

--


-- Errors

throw :: Monad m => ErrorType -> EvaluatorEnv m a
throw eT = lift . lift . throwE $ Error Nothing eT

throwHere :: Monad m => ErrorType -> EvaluatorEnv m a
throwHere eT = do
    l <- getCurrentLocation
    lift . lift . throwE $ Error (Just l) eT

--


-- Auxiliary

changeFunctions :: Monad m => (FunData -> FunData) -> EvaluatorEnv m ()
changeFunctions m = lift $ modify (\(fs, vs, rs, p, me) -> (m fs, vs, rs, p, me))

changeVariables :: Monad m => (VarData -> VarData) -> EvaluatorEnv m ()
changeVariables m = lift $ modify (\(fs, vs, rs, p, me) -> (fs, m vs, rs, p, me))

changeReferences :: Monad m => (RefData -> RefData) -> EvaluatorEnv m ()
changeReferences m = lift $ modify (\(fs, vs, rs, p, me) -> (fs, vs, m rs, p, me))

changePointer :: Monad m => (Int -> Int) -> EvaluatorEnv m ()
changePointer m = lift $ modify (\(fs, vs, rs, p, me) -> (fs, vs, rs, m p, me))

changeMaxMemory :: Monad m => (Int -> Int) -> EvaluatorEnv m ()
changeMaxMemory m = lift $ modify (\(fs, vs, rs, p, me) -> (fs, vs, rs, p, m me))

--


-- Functions

getFunctionCallable :: Monad m => FunId -> EvaluatorEnv m FunCallable
getFunctionCallable fid = lift $ gets (\(fs, _, _, _, _) -> fs M.! fid)

setFunctionCallable :: Monad m => FunId -> FunCallable -> EvaluatorEnv m ()
setFunctionCallable fid f = changeFunctions $ M.insert fid f

setFunctions :: Monad m => [(FunId, FunCallable)] -> EvaluatorEnv m ()
setFunctions fs = changeFunctions $ const (M.fromList fs)

--


-- References

getValueAtAddress :: Monad m => Int -> EvaluatorEnv m (Bare Value)
getValueAtAddress addr = lift $ gets (\(_, _, rs, _, _) -> rs IM.! addr)

setValueAtAddress :: Monad m => Int -> Bare Value -> EvaluatorEnv m ()
setValueAtAddress addr v = changeReferences $ IM.insert addr v

-- Replaces the references to values contained in a value with the referenced values
loadReferences :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
loadReferences (ListV _ eT refs) = ListV () eT <$> mapM loadReferences refs
loadReferences (RefV _ addr) = getValueAtAddress addr >>= loadReferences
loadReferences v = return v

-- Defines a new value at the next empty position and returns its address
addValue :: Monad m => Bare Value -> EvaluatorEnv m Int
addValue v = do
    p <- lift $ gets (\(_, _, _, p, _) -> p)
    setValueAtAddress p v
    changePointer (+1)
    return p

--


-- Variables

variableIsDefined :: Monad m => Name -> EvaluatorEnv m Bool
variableIsDefined vn = lift $ gets (\(_, vs:_, _, _, _) -> M.member vn vs)

getVariableAddress :: Monad m => Name -> EvaluatorEnv m Int
getVariableAddress vn = lift $ gets (\(_, vs:_, _, _, _) -> vs M.! vn)

setVariableAddress :: Monad m => Name -> Int -> EvaluatorEnv m ()
setVariableAddress vn addr = changeVariables (\(vs:vss) -> M.insert vn addr vs : vss)

removeVariable :: Monad m => Name -> EvaluatorEnv m ()
removeVariable vn = changeVariables (\(vs:vss) -> M.delete vn vs : vss)

getVariableValue :: Monad m => Name -> EvaluatorEnv m (Bare Value)
getVariableValue vn = getVariableAddress vn >>= getValueAtAddress

setVariableValue :: Monad m => Name -> Bare Value -> EvaluatorEnv m ()
setVariableValue vn v = do
    isDef <- variableIsDefined vn
    if isDef
        then getVariableAddress vn >>= (`setValueAtAddress` v)
        else addValue v >>= setVariableAddress vn

-- Receives a list of new variables to be declared and a list of references to be set and performs an action with those variables
-- New variables are discarded after the action
-- The original values of the variables referenced can be modified inside the action
inNewScope :: Monad m => EvaluatorEnv m a -> [(Name, Bare Value)] -> [(Name, Int)] -> EvaluatorEnv m a
inNewScope action newVarVals newVarRefs = do
    -- Create a new empty scope
    changeVariables (M.empty:)
    -- Populate it with the given variables
    mapM_ (uncurry setVariableValue) newVarVals
    mapM_ (uncurry setVariableAddress) newVarRefs
    -- Perform the action using only the given variables
    r <- action
    -- Restore the state and return the result
    changeVariables tail
    return r

inContainedScope :: Monad m => EvaluatorEnv m a -> EvaluatorEnv m a
inContainedScope action = do
    vars <- lift $ M.keysSet . head <$> gets (\(_, vs, _, _, _) -> vs)
    r <- action
    changeVariables (\(vs:vss) -> M.restrictKeys vs vars : vss)
    return r

--


-- Garbage collector

getUsedMemory :: Monad m => EvaluatorEnv m Int
getUsedMemory = lift $ gets (\(_, _, rs, _, _) -> IM.size rs)

-- Starts the garbage collector if neccessary
tick :: Monad m => EvaluatorEnv m ()
tick = do
    usedMem <- getUsedMemory
    maxMem <- lift $ gets (\(_, _, _, _, me) -> me)
    when (usedMem >= maxMem) collectGarbage

collectGarbage :: Monad m => EvaluatorEnv m ()
collectGarbage = do
    rAddrs <- getReachableAddresses
    let keysSet = IS.fromList rAddrs
    changeReferences (`IM.restrictKeys` keysSet)
    usedMem <- getUsedMemory
    changeMaxMemory $ const (usedMem * 2)

getReachableAddresses :: Monad m => EvaluatorEnv m [Int]
getReachableAddresses = do
    scopes <- lift $ gets (\(_, vs, _, _, _) -> vs)
    let varAddrs = concatMap M.elems scopes
    varVals <- mapM getValueAtAddress varAddrs
    valsAddrs <- concat <$> mapM getReachableFromValue varVals
    return (varAddrs ++ valsAddrs)
    where
        getReachableFromValue :: Monad m => Bare Value -> EvaluatorEnv m [Int]
        getReachableFromValue (RefV _ addr) = do
            v <- getValueAtAddress addr
            (addr:) <$> getReachableFromValue v
        getReachableFromValue (ListV _ _ refs) = concat <$> mapM getReachableFromValue refs
        getReachableFromValue _ = return []

--
