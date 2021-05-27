{-# LANGUAGE RankNTypes #-}

module BuiltInEval where

import Control.Monad.Trans.Class ( lift )
import Control.Monad ( void )
import Data.List ( intercalate )
import Data.Maybe ( fromJust )

import PrettyPrinter
import EvaluatorEnv
import Errors
import AST

--


-- Auxiliary

io :: IO () -> EvaluatorEnv ()
io = lift . lift . lift

binaryOperation :: (forall a. (Num a) => a -> a -> a) -> Value b -> Value c -> Bare Value
binaryOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
binaryOperation op (IntV _ n1) (IntV _ n2) = IntV () $ n1 `op` n2

floatOperation :: (Float -> Float -> Float) -> Value a -> Value b -> Bare Value
floatOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
floatOperation op (IntV _ n1) (IntV _ n2) = FloatV () $ fromIntegral n1 `op` fromIntegral n2

listOperation :: ([Value a] -> [Value b] -> [Value c]) -> Value a -> Value b -> Bare Value
listOperation op (ListV _ t1 vs1) (ListV _ _ vs2) = ListV () t1 $ map void (vs1 `op` vs2)

relationalOperation :: (forall a. (Ord a) => a -> a -> Bool) -> Value a -> Value b -> Bare Value
relationalOperation op (IntV _ n) (FloatV _ f) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f) (IntV _ n) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f1) (FloatV _ f2) = BoolV () $ f1 `op` f2
relationalOperation op (IntV _ n1) (IntV _ n2) = BoolV () $ n1 `op` n2

binaryModification :: (forall a. (Num a) => a -> a -> a) -> Value b -> Value c -> EvaluatorEnv ()
binaryModification op (VarV _ vn) v = do
    varV <- fromJust <$> getVariableValue vn
    let newVal = binaryOperation op varV v
    setVariableValue vn newVal

floatModification :: (Float -> Float -> Float) -> Value b -> Value c -> EvaluatorEnv ()
floatModification op (VarV _ vn) v = do
    varV <- fromJust <$> getVariableValue vn
    let newVal = floatOperation op varV v
    setVariableValue vn newVal

listModification :: ([Bare Value] -> [Bare Value] -> [Bare Value]) -> Value a -> Value b -> EvaluatorEnv ()
listModification op (VarV _ vn) v = do
    varV <- fromJust <$> getVariableValue vn
    let newVal = listOperation op varV (void v)
    setVariableValue vn newVal

--


-- Operator evaluators

evaluateBuiltInOperator :: FunId -> [Value a] -> EvaluatorEnv (Bare Value)
evaluateBuiltInOperator "%_plus_%" [v1, v2] = evaluatePlus v1 v2
evaluateBuiltInOperator "%_times_%" [v1, v2] = evaluateTimes v1 v2
evaluateBuiltInOperator "%_minus_%" [v1, v2] = evaluateMinus v1 v2
evaluateBuiltInOperator "%_divided_by_%" [v1, v2] = evaluateDividedBy v1 v2
evaluateBuiltInOperator "%_is_equal_to_%" [v1, v2] = evaluateIsEqualTo v1 v2
evaluateBuiltInOperator "%_is_not_equal_to_%" [v1, v2] = evaluateIsNotEqualTo v1 v2
evaluateBuiltInOperator "%_is_less_than_%" [v1, v2] = evaluateIsLessThan v1 v2
evaluateBuiltInOperator "%_is_less_than_or_equal_to_%" [v1, v2] = evaluateIsLessThanOrEqualTo v1 v2
evaluateBuiltInOperator "%_is_greater_than_%" [v1, v2] = evaluateIsGreaterThan v1 v2
evaluateBuiltInOperator "%_is_greater_than_or_equal_to_%" [v1, v2] = evaluateIsGreaterThanOrEqualTo v1 v2
evaluateBuiltInOperator "the_element_of_%_at_%" [l, v] = evaluateElementOfListAt l v
evaluateBuiltInOperator "%_appended_to_%" [l1, l2] = evaluateAppendedTo l1 l2
evaluateBuiltInOperator "the_list_from_%_to_%" [v1, v2] = evaluateTheListFromTo v1 v2

evaluatePlus :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluatePlus v1 v2 = return $ binaryOperation (+) v1 v2

evaluateTimes :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateTimes v1 v2 = return $ binaryOperation (*) v1 v2

evaluateMinus :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateMinus v1 v2 = return $ binaryOperation (-) v1 v2

evaluateDividedBy :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateDividedBy _ (IntV _ 0) = throwHere DivisionByZero
evaluateDividedBy _ (FloatV _ 0) = throwHere DivisionByZero
evaluateDividedBy v1 v2 = return $ floatOperation (/) v1 v2

evaluateIsEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsEqualTo v1 v2 = return $ relationalOperation (==) v1 v2

evaluateIsNotEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsNotEqualTo v1 v2 = return $ relationalOperation (/=) v1 v2

evaluateIsLessThan :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsLessThan v1 v2 = return $ relationalOperation (<) v1 v2

evaluateIsLessThanOrEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsLessThanOrEqualTo v1 v2 = return $ relationalOperation (<=) v1 v2

evaluateIsGreaterThan :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsGreaterThan v1 v2 = return $ relationalOperation (>) v1 v2

evaluateIsGreaterThanOrEqualTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateIsGreaterThanOrEqualTo v1 v2 = return $ relationalOperation (>=) v1 v2

evaluateElementOfListAt :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateElementOfListAt (ListV _ _ []) _ = throwHere EmptyList
evaluateElementOfListAt (ListV _ _ xs) (IntV _ n)
    | n < 0 = throwHere $ OutOfBoundsIndex n
    | n < length xs = return . void $ xs !! n
    | otherwise = throwHere $ OutOfBoundsIndex n

evaluateAppendedTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateAppendedTo v1 v2 = return $ listOperation (++) v1 v2

evaluateTheListFromTo :: Value a -> Value a -> EvaluatorEnv (Bare Value)
evaluateTheListFromTo (IntV _ vF) (IntV _ vT) = return $ ListV () IntT [IntV () n | n <- [vF..vT]]

--


-- Procedure evaluators

evaluateBuiltInProcedure :: FunId -> [Value a] -> EvaluatorEnv ()
evaluateBuiltInProcedure "print_%" [v] = evaluatePrint v
evaluateBuiltInProcedure "swap_%_with_%" [v1, v2] = evaluateSwapWith v1 v2
evaluateBuiltInProcedure "add_%_to_%" [v1, v2] = evaluateAddTo v1 v2
evaluateBuiltInProcedure "multiply_%_by_%" [v1, v2] = evaluateMultiplyBy v1 v2
evaluateBuiltInProcedure "subtract_%_from_%" [v1, v2] = evaluateSubtractFrom v1 v2
evaluateBuiltInProcedure "divide_%_by_%" [v1, v2] = evaluateDivideBy v1 v2
evaluateBuiltInProcedure "append_%_to_%" [v1, v2] = evaluateAppendTo v1 v2
evaluateBuiltInProcedure x y = error $ show x ++ show (length y)

evaluatePrint :: Value a -> EvaluatorEnv ()
evaluatePrint = io . putStr . ppValue

evaluateSwapWith :: Value a -> Value a -> EvaluatorEnv ()
evaluateSwapWith (VarV _ vn1) (VarV _ vn2) = do
    v1 <- fromJust <$> getVariableValue vn1
    v2 <- fromJust <$> getVariableValue vn2
    setVariableValue vn1 v2
    setVariableValue vn2 v1

evaluateAddTo :: Value a -> Value a -> EvaluatorEnv ()
evaluateAddTo v var = binaryModification (+) var v

evaluateMultiplyBy :: Value a -> Value a -> EvaluatorEnv ()
evaluateMultiplyBy = binaryModification (*)

evaluateSubtractFrom :: Value a -> Value a -> EvaluatorEnv ()
evaluateSubtractFrom v var = binaryModification (-) var v

evaluateDivideBy :: Value a -> Value a -> EvaluatorEnv ()
evaluateDivideBy _ (IntV _ 0) = throwHere DivisionByZero
evaluateDivideBy _ (FloatV _ 0) = throwHere DivisionByZero
evaluateDivideBy var v = floatModification (/) var v

evaluateAppendTo :: Value a -> Value a -> EvaluatorEnv ()
evaluateAppendTo v var = listModification (++) var v

--
