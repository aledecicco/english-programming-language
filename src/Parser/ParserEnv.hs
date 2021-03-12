module ParserEnv (
    module ParserEnv,
    setFunction, getFunction, functionIsDefined,
    variableIsDefined,
    setLineNumber, getLineNumber
    ) where

import Env
import AST

--


-- Aliases

-- Store function signatures and variable types
type ParserEnv a = Env Function Type a

setVariableType :: Name -> Type -> ParserEnv ()
setVariableType = setVariable

getVariableType :: Name -> ParserEnv (Maybe Type)
getVariableType = getVariable

resetVariableTypes :: ParserEnv ()
resetVariableTypes = resetVariables

getFunctions :: ParserEnv [Function]
getFunctions = map snd <$> getFunEnv

--
