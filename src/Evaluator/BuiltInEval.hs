{-# LANGUAGE RankNTypes #-}

module BuiltInEval where

import Control.Monad.Trans.Class ( lift )
import Data.List ( intercalate )

import EvaluatorEnv
import Errors
import AST

--


-- Auxiliary

io :: IO () -> EvaluatorEnv ()
io = lift . lift

binaryOperation :: (forall a. (Num a) => a -> a -> a) -> Value -> Value -> Value
binaryOperation op (IntV n) (FloatV f) = FloatV $ fromIntegral n `op` f
binaryOperation op (FloatV f) (IntV n) = FloatV $ fromIntegral n `op` f
binaryOperation op (FloatV f1) (FloatV f2) = FloatV $ f1 `op` f2
binaryOperation op (IntV n1) (IntV n2) = IntV $ n1 `op` n2

floatOperation :: (Float -> Float -> Float) -> Value -> Value -> Value
floatOperation op (IntV n) (FloatV f) = FloatV $ fromIntegral n `op` f
floatOperation op (FloatV f) (IntV n) = FloatV $ fromIntegral n `op` f
floatOperation op (FloatV f1) (FloatV f2) = FloatV $ f1 `op` f2
floatOperation op (IntV n1) (IntV n2) = FloatV $ fromIntegral n1 `op` fromIntegral n2

relationalOperation :: (forall a. (Ord a) => a -> a -> Bool) -> Value -> Value -> Value
relationalOperation op (IntV n) (FloatV f) = BoolV $ fromIntegral n `op` f
relationalOperation op (FloatV f) (IntV n) = BoolV $ fromIntegral n `op` f
relationalOperation op (FloatV f1) (FloatV f2) = BoolV $ f1 `op` f2
relationalOperation op (IntV n1) (IntV n2) = BoolV $ n1 `op` n2

--

-- Evaluators

evaluateBuiltInOperator :: FunctionId -> [Value] -> EvaluatorEnv Value
evaluateBuiltInOperator "%_plus_%" [v1, v2] = evaluatePlus v1 v2
evaluateBuiltInOperator "%_times_%" [v1, v2] = evaluateTimes v1 v2
evaluateBuiltInOperator "%_minus_%" [v1, v2] = evaluateMinus v1 v2
evaluateBuiltInOperator "%_divided_by_%" [v1, v2] = evaluateDividedBy v1 v2
evaluateBuiltInOperator "%_is_less_than_%" [v1, v2] = evaluateIsLessThan v1 v2
evaluateBuiltInOperator "%_is_less_than_or_equal_to_%" [v1, v2] = evaluateIsLessThanOrEqualTo v1 v2
evaluateBuiltInOperator "%_is_greater_than_%" [v1, v2] = evaluateIsGreaterThan v1 v2
evaluateBuiltInOperator "%_is_greater_than_or_equal_to_%" [v1, v2] = evaluateIsGreaterThanOrEqualTo v1 v2
evaluateBuiltInOperator "the_element_of_%_at_position_%" [l, v] = evaluateElementOfListAtPosition l v
evaluateBuiltInOperator "%_appended_to_%" [l1, l2] = evaluateListAppendedToList l1 l2

evaluateBuiltInProcedure :: FunctionId -> [Value] -> EvaluatorEnv ()
evaluateBuiltInProcedure "print_%" [v] = evaluatePrint v

evaluatePrint :: Value -> EvaluatorEnv ()
evaluatePrint v = io . putStr $ evaluatePrint' v
    where
        evaluatePrint' :: Value -> String
        evaluatePrint' (IntV n) = show n
        evaluatePrint' (FloatV f) = show f
        evaluatePrint' (CharV c) = [c]
        evaluatePrint' (BoolV b) = show b
        evaluatePrint' (ListV CharT l) = concatMap evaluatePrint' l
        evaluatePrint' (ListV _ l) = "[" ++ intercalate ", " (map evaluatePrint' l) ++ "]"

evaluatePlus :: Value -> Value -> EvaluatorEnv Value
evaluatePlus v1 v2 = return $ binaryOperation (+) v1 v2

evaluateTimes :: Value -> Value -> EvaluatorEnv Value
evaluateTimes v1 v2 = return $ binaryOperation (*) v1 v2

evaluateMinus :: Value -> Value -> EvaluatorEnv Value
evaluateMinus v1 v2 = return $ binaryOperation (-) v1 v2

evaluateDividedBy :: Value -> Value -> EvaluatorEnv Value
evaluateDividedBy v1 v2 = return $ floatOperation (/) v1 v2

evaluateElementOfListAtPosition :: Value -> Value -> EvaluatorEnv Value
evaluateElementOfListAtPosition (ListV _ []) _ = emptyListError
evaluateElementOfListAtPosition (ListV _ xs) (IntV n)
    | n < 0 = outOfBoundsIndexError n
    | n < length xs = return $ xs !! n
    | otherwise = outOfBoundsIndexError n

evaluateListAppendedToList :: Value -> Value -> EvaluatorEnv Value
evaluateListAppendedToList (ListV t xs) (ListV _ ys) = return $ ListV t (xs ++ ys)

evaluateIsLessThan :: Value -> Value -> EvaluatorEnv Value
evaluateIsLessThan v1 v2 = return $ relationalOperation (<) v1 v2

evaluateIsLessThanOrEqualTo :: Value -> Value -> EvaluatorEnv Value
evaluateIsLessThanOrEqualTo v1 v2 = return $ relationalOperation (<=) v1 v2

evaluateIsGreaterThan :: Value -> Value -> EvaluatorEnv Value
evaluateIsGreaterThan v1 v2 = return $ relationalOperation (>) v1 v2

evaluateIsGreaterThanOrEqualTo :: Value -> Value -> EvaluatorEnv Value
evaluateIsGreaterThanOrEqualTo v1 v2 = return $ relationalOperation (>=) v1 v2

--
