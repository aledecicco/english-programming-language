module EvaluatorEnv (
    module EvaluatorEnv,
    setLineNumber, getLineNumber,
    Error
    ) where

import Control.Monad.Trans.State ( runStateT )
import Control.Monad.Trans.Except ( runExceptT )

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

setFunctionSentences :: FunctionId -> [SentenceLine] -> EvaluatorEnv ()
setFunctionSentences = setFunction

getFunctionSentences :: FunctionId -> EvaluatorEnv (Maybe [SentenceLine])
getFunctionSentences = getFunction

runEvaluatorEnv :: EvaluatorEnv r -> EvaluatorState -> IO (Either Error (r, EvaluatorState))
runEvaluatorEnv f e = runExceptT (runStateT f e)

--
