module ParserEnv (
    module ParserEnv,
    setFunction, getFunction, functionIsDefined,
    variableIsDefined,
    setLineNumber, getLineNumber,
    Error
    ) where

import Control.Monad.Identity ( runIdentity, Identity )
import Control.Monad.Trans.State ( runStateT )
import Control.Monad.Trans.Except ( runExceptT )

import Env
import AST

--


--

type ParserEnv a = Env Function Type Identity a
type ParserState = EnvData Function Type

setVariableType :: Name -> Type -> ParserEnv ()
setVariableType = setVariable

getVariableType :: Name -> ParserEnv (Maybe Type)
getVariableType = getVariable

resetVariableTypes :: ParserEnv ()
resetVariableTypes = resetVariables

getFunctions :: ParserEnv [Function]
getFunctions = map snd <$> getFunEnv

runParserEnv :: ParserEnv r -> ParserState -> Either Error (r, ParserState)
runParserEnv f e = runIdentity $ runExceptT (runStateT f e)

--
