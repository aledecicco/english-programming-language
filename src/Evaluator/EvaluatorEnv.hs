module EvaluatorEnv ( module EvaluatorEnv, setCurrentLocation, withLocation, initialLocation ) where

import Data.List ( find )
import Control.Monad.Trans.Class ( lift )
import Control.Monad.Trans.State ( gets, modify, runStateT, StateT )
import Control.Monad.Trans.Except ( throwE, runExceptT, ExceptT )

import PrettyPrinter ( ppError )
import Location
import AST

--


-- Type definition

type FunData = (FunId, FunCallable)
type Reference = (Name, Int)
type EvaluatorData = ([FunData], [Reference], [Bare Value], Int)
type EvaluatorEnv a = LocationT (StateT EvaluatorData (ExceptT String IO)) a

runEvaluatorEnv :: EvaluatorEnv a -> EvaluatorData -> Location -> IO (Either String ((a, Location), EvaluatorData))
runEvaluatorEnv f d s = runExceptT $ runStateT (runLocationT f s) d

initialState :: EvaluatorData
initialState = ([], [], [], 0)

--


-- Errors

throw :: [String] -> EvaluatorEnv a
throw ps = do
    l <- getCurrentLocation
    lift . lift . throwE $ ppError ps l

--


-- Auxiliary

removeById :: Eq a => a -> [(a, b)] -> [(a, b)]
removeById id = filter (\p -> fst p /= id)

changeFunctions :: ([FunData] -> [FunData]) -> EvaluatorEnv ()
changeFunctions m = lift $ modify (\(fs, rs, vs, p) -> (m fs, rs, vs, p))

changeReferences :: ([Reference] -> [Reference]) -> EvaluatorEnv ()
changeReferences m = lift $ modify (\(fs, rs, vs, p) -> (fs, m rs, vs, p))

changeValues :: ([Bare Value] -> [Bare Value]) -> EvaluatorEnv ()
changeValues m = lift $ modify (\(fs, rs, vs, p) -> (fs, rs, m vs, p))

changePointer :: (Int -> Int) -> EvaluatorEnv ()
changePointer m = lift $ modify (\(fs, rs, vs, p) -> (fs, rs, vs, m p))

--


-- Functions

getFunctionCallable :: FunId -> EvaluatorEnv (Maybe FunCallable)
getFunctionCallable fid = lift $ gets (\(fs, _, _, _) -> lookup fid fs)

setFunctionCallable :: FunId -> FunCallable -> EvaluatorEnv ()
setFunctionCallable fid f = do
    r <- getFunctionCallable fid
    removeFunctionCallable fid
    changeFunctions ((fid, f):)

removeFunctionCallable :: FunId -> EvaluatorEnv ()
removeFunctionCallable fid = changeFunctions $ removeById fid

setFunctions :: [(FunId, FunCallable)] -> EvaluatorEnv ()
setFunctions = changeFunctions . const

--


-- References

getVariableAddress :: Name -> EvaluatorEnv (Maybe Int)
getVariableAddress vn = lift $ gets (\(_, vas, _, _) -> lookup vn vas)

setVariableAddress :: Name -> Int -> EvaluatorEnv ()
setVariableAddress vn addr = do
    removeVariableAddress vn
    changeReferences ((vn, addr):)

removeVariableAddress :: Name -> EvaluatorEnv ()
removeVariableAddress vn = changeReferences $ removeById vn

--


-- Values

getValueAtAddress :: Int -> EvaluatorEnv (Bare Value)
getValueAtAddress addr = lift $ gets (\(_, _, avs, _) -> avs !! addr)

setValueAtAddress :: Int -> Bare Value -> EvaluatorEnv ()
setValueAtAddress addr v = changeValues $ replaceNth addr v
    where
        replaceNth :: Int -> a -> [a] -> [a]
        replaceNth n v l = take n l ++ [v] ++ drop (n+1) l

--


-- Stack pointer

getStackPointer :: EvaluatorEnv Int
getStackPointer = lift $ gets (\(_, _, _, p) -> p)

setStackPointer :: Int -> EvaluatorEnv ()
setStackPointer p = changePointer $ const p

--


-- Variables

getVariableValue :: Name -> EvaluatorEnv (Maybe (Bare Value))
getVariableValue vn = do
    r <- getVariableAddress vn
    case r of
        Just addr -> Just <$> getValueAtAddress addr
        Nothing -> return Nothing

addVariableValue :: Name -> Bare Value -> EvaluatorEnv ()
addVariableValue vn v = do
    p <- getStackPointer
    setVariableAddress vn p
    setValueAtAddress p v
    setStackPointer (p+1)

setVariableValue :: Name -> Bare Value -> EvaluatorEnv ()
setVariableValue vn v = do
    r <- getVariableAddress vn
    case r of
        Just addr -> setValueAtAddress addr v
        Nothing -> addVariableValue vn v

-- ToDo: garbage collection of value at address
removeVariableValue :: Name -> EvaluatorEnv ()
removeVariableValue = removeVariableAddress

-- Receives a list of new variables to be declared and a list of references to be set and performs an action with those variables
-- The values of new variables are discarded after the action
-- The original values of the variables references can be modified inside the action
withVariables :: EvaluatorEnv a -> [(Name, Bare Value)] -> [(Name, Int)] -> EvaluatorEnv a
withVariables action newVarVals newVarRefs = do
    let newVarsLen = length newVarVals
    varRefs <- lift $ gets (\(_, vas, _, _) -> vas)
    changeReferences $ const []
    mapM_ (uncurry addVariableValue) newVarVals
    mapM_ (uncurry setVariableAddress) newVarRefs
    r <- action
    changeReferences $ const varRefs
    changeValues $ drop newVarsLen
    changePointer $ subtract newVarsLen
    return r

--
