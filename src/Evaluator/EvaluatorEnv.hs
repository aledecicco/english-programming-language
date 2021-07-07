module EvaluatorEnv ( module EvaluatorEnv, setCurrentLocation, getCurrentLocation, withLocation, initialLocation ) where

import Data.List ( find )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( gets, modify, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, runExceptT, ExceptT )

import Errors
import Location
import AST

--


-- Type definitions

type FunData = (FunId, FunCallable)
type Reference = (Name, Int)

-- Function callables
-- A mapping between variable names and their addresses
-- A list of values, where the index of each value is its address
-- The stack pointer
type EvaluatorData = ([FunData], [Reference], [Bare Value], Int)
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
initialState = ([], [], [], 0)

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

removeById :: Eq a => a -> [(a, b)] -> [(a, b)]
removeById id = filter (\p -> fst p /= id)

changeFunctions :: Monad m => ([FunData] -> [FunData]) -> EvaluatorEnv m ()
changeFunctions m = lift $ modify (\(fs, rs, vs, p) -> (m fs, rs, vs, p))

changeReferences :: Monad m => ([Reference] -> [Reference]) -> EvaluatorEnv m ()
changeReferences m = lift $ modify (\(fs, rs, vs, p) -> (fs, m rs, vs, p))

changeValues :: Monad m => ([Bare Value] -> [Bare Value]) -> EvaluatorEnv m ()
changeValues m = lift $ modify (\(fs, rs, vs, p) -> (fs, rs, m vs, p))

changePointer :: Monad m => (Int -> Int) -> EvaluatorEnv m ()
changePointer m = lift $ modify (\(fs, rs, vs, p) -> (fs, rs, vs, m p))

--


-- Functions

getFunctionCallable :: Monad m => FunId -> EvaluatorEnv m (Maybe FunCallable)
getFunctionCallable fid = lift $ gets (\(fs, _, _, _) -> lookup fid fs)

setFunctionCallable :: Monad m => FunId -> FunCallable -> EvaluatorEnv m ()
setFunctionCallable fid f = do
    r <- getFunctionCallable fid
    removeFunctionCallable fid
    changeFunctions ((fid, f):)

removeFunctionCallable :: Monad m => FunId -> EvaluatorEnv m ()
removeFunctionCallable fid = changeFunctions $ removeById fid

setFunctions :: Monad m => [(FunId, FunCallable)] -> EvaluatorEnv m ()
setFunctions = changeFunctions . const

--


-- References

getVariableAddress :: Monad m => Name -> EvaluatorEnv m (Maybe Int)
getVariableAddress vn = lift $ gets (\(_, vas, _, _) -> lookup vn vas)

setVariableAddress :: Monad m => Name -> Int -> EvaluatorEnv m ()
setVariableAddress vn addr = do
    removeVariableAddress vn
    changeReferences ((vn, addr):)

removeVariableAddress :: Monad m => Name -> EvaluatorEnv m ()
removeVariableAddress vn = changeReferences $ removeById vn

--


-- Values

getValueAtAddress :: Monad m => Int -> EvaluatorEnv m (Bare Value)
getValueAtAddress addr = lift $ gets (\(_, _, avs, _) -> avs !! addr)

-- Replaces the references to values contained in a value with the referenced values
loadReferences :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
loadReferences (ListV _ t refs) = ListV () t <$> mapM loadReferences refs
loadReferences (RefV _ addr) = getValueAtAddress addr >>= loadReferences
loadReferences v = return v

-- Replaces the values contained in a value with references to those values, saving them to memory first
saveReferences :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
saveReferences (ListV _ t es) = ListV () t <$> mapM ((RefV () <$>) . addValue) es
saveReferences v = return v

setValueAtAddress :: Monad m => Int -> Bare Value -> EvaluatorEnv m ()
setValueAtAddress addr v = changeValues $ replaceNth addr v
    where
        replaceNth :: Int -> a -> [a] -> [a]
        replaceNth n v l = take n l ++ [v] ++ drop (n+1) l

-- Defines a new value at the next empty position and returns its address
addValue :: Monad m => Bare Value -> EvaluatorEnv m Int
addValue v = do
    p <- getStackPointer
    setValueAtAddress p v
    setStackPointer (p+1)
    return p

--


-- Stack pointer

getStackPointer :: Monad m => EvaluatorEnv m Int
getStackPointer = lift $ gets (\(_, _, _, p) -> p)

setStackPointer :: Monad m => Int -> EvaluatorEnv m ()
setStackPointer p = changePointer $ const p

--


-- Variables

getVariableValue :: Monad m => Name -> EvaluatorEnv m (Maybe (Bare Value))
getVariableValue vn = do
    r <- getVariableAddress vn
    case r of
        Just addr -> Just <$> getValueAtAddress addr
        Nothing -> return Nothing

addVariableValue :: Monad m => Name -> Bare Value -> EvaluatorEnv m ()
addVariableValue vn v = do
    addr <- addValue v
    setVariableAddress vn addr

setVariableValue :: Monad m => Name -> Bare Value -> EvaluatorEnv m ()
setVariableValue vn v = do
    r <- getVariableAddress vn
    case r of
        Just addr -> setValueAtAddress addr v
        Nothing -> addVariableValue vn v

removeVariableValue :: Monad m => Name -> EvaluatorEnv m ()
removeVariableValue = removeVariableAddress

-- Receives a list of new variables to be declared and a list of references to be set and performs an action with those variables
-- The values of new variables are discarded after the action
-- The original values of the variables references can be modified inside the action
withVariables :: Monad m => EvaluatorEnv m (Maybe (Bare Value)) -> [(Name, Bare Value)] -> [(Name, Int)] -> EvaluatorEnv m (Maybe (Bare Value))
withVariables action newVarVals newVarRefs = do
    -- Save current state
    varRefs <- lift $ gets (\(_, vas, _, _) -> vas)
    varValsLen <- lift $ length <$> gets (\(_, _, vals, _) -> vals)
    p <- lift $ gets (\(_, _, _, p) -> p)
    -- Perform the action using only the given variables
    changeReferences $ const []
    mapM_ (uncurry addVariableValue) newVarVals
    mapM_ (uncurry setVariableAddress) newVarRefs
    r <- action
    -- Load values referenced in the result before cleaning the stack
    r' <- case r of
        Just v -> Just <$> loadReferences v
        Nothing -> return Nothing
    -- Restore the state
    changeReferences $ const varRefs
    changeValues $ take varValsLen
    changePointer $ const p
    -- Save loaded values back to the stack and return the result
    case r' of
        Just v' -> Just <$> saveReferences v'
        Nothing -> return Nothing

--
