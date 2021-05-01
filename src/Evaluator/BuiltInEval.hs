{-# LANGUAGE RankNTypes #-}

module BuiltInEval where

import Control.Monad.Trans.Class ( lift )
import Control.Monad ( void )
import Data.List ( intercalate )

import PrettyPrinter
import EvaluatorEnv
import Errors
import AST

--


-- Auxiliary

io :: IO () -> EvaluatorEnv ()
io = lift . lift . lift

binaryOperation :: (forall a. (Num a) => a -> a -> a) -> Value b -> Value b -> Bare Value
binaryOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
binaryOperation op (IntV _ n1) (IntV _ n2) = IntV () $ n1 `op` n2

floatOperation :: (Float -> Float -> Float) -> Value a -> Value a -> Bare Value
floatOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
floatOperation op (IntV _ n1) (IntV _ n2) = FloatV () $ fromIntegral n1 `op` fromIntegral n2

relationalOperation :: (forall a. (Ord a) => a -> a -> Bool) -> Value a -> Value a -> Bare Value
relationalOperation op (IntV _ n) (FloatV _ f) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f) (IntV _ n) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f1) (FloatV _ f2) = BoolV () $ f1 `op` f2
relationalOperation op (IntV _ n1) (IntV _ n2) = BoolV () $ n1 `op` n2

--

-- Evaluators

evaluateBuiltInOperator :: FunId -> [Value a] -> EvaluatorEnv (Bare Value)
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

evaluateBuiltInProcedure :: FunId -> [Value a] -> EvaluatorEnv ()
evaluateBuiltInProcedure "print_%" [v] = evaluatePrint v

evaluatePrint :: Value a -> EvaluatorEnv ()
evaluatePrint = io . putStr . ppValue

evaluatePlus :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluatePlus v1 v2 = return $ binaryOperation (+) v1 v2

evaluateTimes :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateTimes v1 v2 = return $ binaryOperation (*) v1 v2

evaluateMinus :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateMinus v1 v2 = return $ binaryOperation (-) v1 v2

evaluateDividedBy :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateDividedBy _ (IntV _ 0) = throw divisionByZeroError
evaluateDividedBy _ (FloatV _ 0) = throw divisionByZeroError
evaluateDividedBy v1 v2 = return $ floatOperation (/) v1 v2

evaluateElementOfListAtPosition :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateElementOfListAtPosition (ListV _ _ []) _ = throw emptyListError
evaluateElementOfListAtPosition (ListV _ _ xs) (IntV _ n)
    | n < 0 = throw $ outOfBoundsIndexError n
    | n < length xs = return . void $ xs !! n
    | otherwise = throw $ outOfBoundsIndexError n

evaluateListAppendedToList :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateListAppendedToList (ListV _ t xs) (ListV _ _ ys) = return $ ListV () t (map void $ xs ++ ys)

evaluateIsLessThan :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsLessThan v1 v2 = return $ relationalOperation (<) v1 v2

evaluateIsLessThanOrEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsLessThanOrEqualTo v1 v2 = return $ relationalOperation (<=) v1 v2

evaluateIsGreaterThan :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsGreaterThan v1 v2 = return $ relationalOperation (>) v1 v2

evaluateIsGreaterThanOrEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsGreaterThanOrEqualTo v1 v2 = return $ relationalOperation (>=) v1 v2

--
