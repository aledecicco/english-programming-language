module ParserEnv (
    module ParserEnv,
    setFunction, getFunction, functionIsDefined,
    variableIsDefined, resetVariables,
    setLineNumber, getLineNumber,
    Error
    ) where

import Control.Monad.Identity ( runIdentity, Identity )

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

removeVariableType :: Name -> ParserEnv ()
removeVariableType = removeVariable

getOperators :: ParserEnv [Function]
getOperators = do
    fs <- map snd <$> getFunEnv
    return $ [f | f@(Function _ (Operator _)) <- fs]

getProcedures :: ParserEnv [Function]
getProcedures = do
    fs <- map snd <$> getFunEnv
    return $ [f | f@(Function _ Procedure) <- fs]

runParserEnv :: ParserEnv r -> ParserState -> Either Error (r, ParserState)
runParserEnv f e = runIdentity $ runEnv f e

--
