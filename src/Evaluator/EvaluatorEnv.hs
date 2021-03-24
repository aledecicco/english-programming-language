module EvaluatorEnv (
    module EvaluatorEnv,
    setLineNumber, getLineNumber,
    Error
    ) where

import Env
import AST

--


--

type EvaluatorEnv a = Env [SentenceLine] Value IO a
type EvaluatorState = EnvData [SentenceLine] Value

setVariableValue :: Name -> Value -> EvaluatorEnv ()
setVariableValue = setVariable

getVariableValue :: Name -> EvaluatorEnv (Maybe Value)
getVariableValue = getVariable

removeVariableValue :: Name -> EvaluatorEnv ()
removeVariableValue = removeVariable

withVariablesReset :: EvaluatorEnv a -> EvaluatorEnv a
withVariablesReset action = do
    vs <- getVarEnv
    r <- action
    setVarEnv vs
    return r

setFunctionSentences :: FunctionId -> [SentenceLine] -> EvaluatorEnv ()
setFunctionSentences = setFunction

getFunctionSentences :: FunctionId -> EvaluatorEnv (Maybe [SentenceLine])
getFunctionSentences = getFunction

runEvaluatorEnv :: EvaluatorEnv r -> EvaluatorState -> IO (Either Error (r, EvaluatorState))
runEvaluatorEnv = runEnv

--
