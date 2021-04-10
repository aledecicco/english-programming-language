module EvaluatorEnv (
    module EvaluatorEnv,
    emptyEnv,
    setLineNumber, getLineNumber,
    Error
    ) where

import Env
import AST

--


--

type EvaluatorEnv a = Env FunCallable Value IO a
type EvaluatorState = EnvData FunCallable Value

setVariableValue :: Name -> Value -> EvaluatorEnv ()
setVariableValue = setVariable

getVariableValue :: Name -> EvaluatorEnv (Maybe Value)
getVariableValue = getVariable

removeVariableValue :: Name -> EvaluatorEnv ()
removeVariableValue = removeVariable

withVariables :: EvaluatorEnv a -> [(Name, Value)] -> EvaluatorEnv a
withVariables action newVars = do
    currentVars <- getVarEnv
    setVarEnv newVars
    r <- action
    setVarEnv currentVars
    return r

setFunctionCallable :: FunId -> FunCallable -> EvaluatorEnv ()
setFunctionCallable = setFunction

getFunctionCallable :: FunId -> EvaluatorEnv (Maybe FunCallable)
getFunctionCallable = getFunction

runEvaluatorEnv :: EvaluatorEnv r -> EvaluatorState -> IO (Either Error (r, EvaluatorState))
runEvaluatorEnv = runEnv

--
