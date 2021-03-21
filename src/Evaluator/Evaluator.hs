module Evaluator where

import Data.List ( find )
import Data.Maybe ( fromJust )
import EvaluatorEnv
import ParserEnv ( ParserState )
import AST

--


-- Auxiliary

translateState :: Program -> ParserState -> EvaluatorState
translateState p (fE, _, _) =
    let funSentences = map (translateFunction p) fE
    in (funSentences, [], 0)
    where
        translateFunction :: Program -> (FunctionId, Function) -> (FunctionId, [SentenceLine])
        translateFunction p (fid, Function t _) =
            let (FunDef _ _ ss) = findDefinition p t
            in (fid, ss)
        findDefinition :: Program -> Title -> Block
        findDefinition p t = fromJust $ find (\(FunDef (Line _ t') _ _) -> t == t') p

--


-- Main

runEvaluator :: EvaluatorEnv r -> EvaluatorState -> IO (Either Error (r, EvaluatorState))
runEvaluator = runEvaluatorEnv

evaluateProgram :: Program -> ParserState -> IO EvaluatorState
evaluateProgram p s = do
    r <- runEvaluator (evaluateProgram' p) (translateState p s)
    case r of
        Left e -> error e
        Right r -> return $ snd r
    where
        evaluateProgram' :: Program -> EvaluatorEnv Program
        evaluateProgram' p = undefined
--
