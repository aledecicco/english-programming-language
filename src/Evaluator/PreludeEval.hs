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

evaluateOperator :: FunctionId -> [Value] -> EvaluatorEnv Value
evaluateOperator "%_plus_%" [v1, v2] = evaluatePlus v1 v2
evaluateOperator "%_times_%" [v1, v2] = evaluateTimes v1 v2
evaluateOperator "the_first_element_of_%" [l] = evaluateFirstItemOfList l
evaluateOperator "%_appended_to_%" [l1, l2] = evaluateListAppendedToList l1 l2

evaluateProcedure :: FunctionId -> [Value] -> EvaluatorEnv ()
evaluateProcedure "print_%" [v] = evaluatePrint v

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

evaluateTimes :: Value -> Value -> EvaluatorEnv Value
evaluateTimes (IntV n) (FloatV f) = return $ FloatV (fromIntegral n * f)
evaluateTimes (FloatV f) (IntV n) = return $ FloatV (fromIntegral n * f)
evaluateTimes (FloatV f1) (FloatV f2) = return $ FloatV (f1 * f2)
evaluateTimes (IntV n1) (IntV n2) = return $ IntV (n1 * n2)

evaluateFirstItemOfList :: Value -> EvaluatorEnv Value
evaluateFirstItemOfList (ListV _ []) = emptyListError
evaluateFirstItemOfList (ListV _ (x:_)) = return x

evaluateListAppendedToList :: Value -> Value -> EvaluatorEnv Value
evaluateListAppendedToList (ListV t xs) (ListV _ ys) = return $ ListV t (xs ++ ys)

--
