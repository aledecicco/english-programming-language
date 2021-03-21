module PreludeEval where

import Control.Monad.Trans.Class ( lift )
import Data.List ( intercalate )

import EvaluatorEnv
import Errors
import AST

--


-- Auxiliary

io :: IO () -> EvaluatorEnv ()
io = lift . lift


--

-- Evaluators

evaluatePrint :: Value -> EvaluatorEnv ()
evaluatePrint v = io . putStr $ evaluatePrint' v
    where
        evaluatePrint' :: Value -> String
        evaluatePrint' (IntV n) = show n
        evaluatePrint' (FloatV f) = show f
        evaluatePrint' (BoolV b) = show b
        evaluatePrint' (ListV _ l) = "[" ++ intercalate ", " (map evaluatePrint' l) ++ "]"

evaluatePlus :: Value -> Value -> EvaluatorEnv Value
evaluatePlus (IntV n) (FloatV f) = return $ FloatV (fromIntegral n + f)
evaluatePlus (FloatV f) (IntV n) = return $ FloatV (fromIntegral n + f)
evaluatePlus (FloatV f1) (FloatV f2) = return $ FloatV (f1 + f2)
evaluatePlus (IntV n1) (IntV n2) = return $ IntV (n1 + n2)

--
