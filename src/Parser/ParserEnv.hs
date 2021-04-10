module ParserEnv (
    module ParserEnv,
    emptyEnv,
    functionIsDefined,
    variableIsDefined, resetVariables,
    setLineNumber, getLineNumber,
    Error
    ) where

import Control.Monad.Identity ( runIdentity, Identity )

import Env
import AST

--


--

type ParserEnv a = Env FunSignature Type Identity a
type ParserState = EnvData FunSignature Type

setVariableType :: Name -> Type -> ParserEnv ()
setVariableType = setVariable

getVariableType :: Name -> ParserEnv (Maybe Type)
getVariableType = getVariable

removeVariableType :: Name -> ParserEnv ()
removeVariableType = removeVariable

setFunctionSignature :: FunId -> FunSignature -> ParserEnv ()
setFunctionSignature = setFunction

getFunctionSignature :: FunId -> ParserEnv (Maybe FunSignature)
getFunctionSignature = getFunction

getOperatorSignatures :: ParserEnv [FunSignature]
getOperatorSignatures = do
    fs <- map snd <$> getFunEnv
    return $ [f | f@(FunSignature _ (Operator _)) <- fs]

getProcedureSignatures :: ParserEnv [FunSignature]
getProcedureSignatures = do
    fs <- map snd <$> getFunEnv
    return $ [f | f@(FunSignature _ Procedure) <- fs]

runParserEnv :: ParserEnv r -> ParserState -> Either Error (r, ParserState)
runParserEnv f e = runIdentity $ runEnv f e

--
