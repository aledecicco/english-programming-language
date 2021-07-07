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

io :: ReadWrite m => m () -> EvaluatorEnv m ()
io = lift . lift . lift

binaryOperation :: (forall a. (Num a) => a -> a -> a) -> Value b -> Value c -> Bare Value
binaryOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
binaryOperation op (IntV _ n1) (IntV _ n2) = IntV () $ n1 `op` n2
binaryOperation _ a b = error "Shouldn't happen: wrong types provided"

floatOperation :: (Float -> Float -> Float) -> Value a -> Value b -> Bare Value
floatOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
floatOperation op (IntV _ n1) (IntV _ n2) = FloatV () $ fromIntegral n1 `op` fromIntegral n2
floatOperation _ _ _ = error "Shouldn't happen: wrong types provided"


listOperation :: ([Value a] -> [Value b] -> [Value c]) -> Value a -> Value b -> Bare Value
listOperation op (ListV _ t1 vs1) (ListV _ _ vs2) = ListV () t1 $ map void (vs1 `op` vs2)
listOperation _ _ _ = error "Shouldn't happen: wrong types provided"

relationalOperation :: (forall a. (Ord a) => a -> a -> Bool) -> Value a -> Value b -> Bare Value
relationalOperation op (IntV _ n) (FloatV _ f) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f) (IntV _ n) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f1) (FloatV _ f2) = BoolV () $ f1 `op` f2
relationalOperation op (IntV _ n1) (IntV _ n2) = BoolV () $ n1 `op` n2
relationalOperation _ _ _ = error "Shouldn't happen: wrong types provided"

binaryModification :: ReadWrite m => (forall a. (Num a) => a -> a -> a) -> Value b -> Value c -> EvaluatorEnv m ()
binaryModification op (RefV _ addr) v = do
    v' <- getValueAtAddress addr
    let newVal = binaryOperation op v' v
    setValueAtAddress addr newVal
binaryModification _ _ _ = error "Shouldn't happen: wrong types provided"

floatModification :: ReadWrite m => (Float -> Float -> Float) -> Value b -> Value c -> EvaluatorEnv m ()
floatModification op (RefV _ addr) v = do
    v' <- getValueAtAddress addr
    let newVal = floatOperation op v' v
    setValueAtAddress addr newVal
floatModification _ _ _ = error "Shouldn't happen: wrong types provided"

listModification :: ReadWrite m => ([Bare Value] -> [Bare Value] -> [Bare Value]) -> Value a -> Value b -> EvaluatorEnv m ()
listModification op (RefV _ addr) v = do
    v' <- getValueAtAddress addr
    let newVal = listOperation op v' (void v)
    setValueAtAddress addr newVal
listModification _ _ _ = error "Shouldn't happen: wrong types provided"

--


-- Operator evaluators

evaluateBuiltInOperator :: ReadWrite m => FunId -> [Value a] -> EvaluatorEnv m (Bare Value)
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
evaluateBuiltInOperator "the_length_of_%" [l] = evaluateLengthOf l
evaluateBuiltInOperator "%_appended_to_%" [l1, l2] = evaluateAppendedTo l1 l2
evaluateBuiltInOperator "the_list_from_%_to_%" [v1, v2] = evaluateTheListFromTo v1 v2
evaluateBuiltInOperator "" _ = error "Shouldn't happen: an operator can't have the empty string as id"
evaluateBuiltInOperator _ _ = error "Shouldn't happen: undefined operator"

evaluatePlus :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluatePlus v1 v2 = return $ binaryOperation (+) v1 v2

evaluateTimes :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateTimes v1 v2 = return $ binaryOperation (*) v1 v2

evaluateMinus :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateMinus v1 v2 = return $ binaryOperation (-) v1 v2

evaluateDividedBy :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateDividedBy _ (IntV _ 0) = throwHere DivisionByZero
evaluateDividedBy _ (FloatV _ 0) = throwHere DivisionByZero
evaluateDividedBy v1 v2 = return $ floatOperation (/) v1 v2

evaluateIsEqualTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsEqualTo (IntV _ n) (FloatV _ f) = return $ BoolV () (fromIntegral n == f)
evaluateIsEqualTo (FloatV _ f) (IntV _ n) = return $ BoolV () (fromIntegral n == f)
evaluateIsEqualTo v1 v2 = return $ BoolV () (void v1 == void v2)

evaluateIsNotEqualTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsNotEqualTo (IntV _ n) (FloatV _ f) = return $ BoolV () (fromIntegral n /= f)
evaluateIsNotEqualTo (FloatV _ f) (IntV _ n) = return $ BoolV () (fromIntegral n /= f)
evaluateIsNotEqualTo v1 v2 = return $ BoolV () (void v1 /= void v2)

evaluateIsLessThan :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsLessThan v1 v2 = return $ relationalOperation (<) v1 v2

evaluateIsLessThanOrEqualTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsLessThanOrEqualTo v1 v2 = return $ relationalOperation (<=) v1 v2

evaluateIsGreaterThan :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsGreaterThan v1 v2 = return $ relationalOperation (>) v1 v2

evaluateIsGreaterThanOrEqualTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateIsGreaterThanOrEqualTo v1 v2 = return $ relationalOperation (>=) v1 v2

evaluateElementOfListAt :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateElementOfListAt (ListV _ _ []) _ = throwHere EmptyList
evaluateElementOfListAt (ListV _ _ xs) (IntV _ n)
    | n < 0 = throwHere $ OutOfBoundsIndex n
    | n < length xs = return . void $ xs !! n
    | otherwise = throwHere $ OutOfBoundsIndex n
evaluateElementOfListAt _ _ = error "Shouldn't happen: wrong types provided"

evaluateLengthOf :: ReadWrite m => Value a -> EvaluatorEnv m (Bare Value)
evaluateLengthOf (ListV _ _ xs) = return $ IntV () (length xs)
evaluateLengthOf _ = error "Shouldn't happen: wrong types provided"

evaluateAppendedTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateAppendedTo v1 v2 = return $ listOperation (++) v1 v2

evaluateTheListFromTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m (Bare Value)
evaluateTheListFromTo (IntV _ vF) (IntV _ vT) = return $ ListV () IntT [IntV () n | n <- [vF..vT]]
evaluateTheListFromTo _ _ = error "Shouldn't happen: wrong types provided"

--


-- Procedure evaluators

evaluateBuiltInProcedure :: ReadWrite m => FunId -> [Value a] -> EvaluatorEnv m ()
evaluateBuiltInProcedure "print_%" [v] = evaluatePrint v
evaluateBuiltInProcedure "swap_%_with_%" [v1, v2] = evaluateSwapWith v1 v2
evaluateBuiltInProcedure "add_%_to_%" [v1, v2] = evaluateAddTo v1 v2
evaluateBuiltInProcedure "multiply_%_by_%" [v1, v2] = evaluateMultiplyBy v1 v2
evaluateBuiltInProcedure "subtract_%_from_%" [v1, v2] = evaluateSubtractFrom v1 v2
evaluateBuiltInProcedure "divide_%_by_%" [v1, v2] = evaluateDivideBy v1 v2
evaluateBuiltInProcedure "append_%_to_%" [v1, v2] = evaluateAppendTo v1 v2
evaluateBuiltInProcedure x y = error $ show x ++ show (length y)

evaluatePrint :: ReadWrite m => Value a -> EvaluatorEnv m ()
evaluatePrint v = do
    v' <- loadReferences $ void v
    (io . write . ppValue) v'

evaluateSwapWith :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateSwapWith (RefV _ addr1) (RefV _ addr2) = do
    v1 <- getValueAtAddress addr1
    v2 <- getValueAtAddress addr2
    setValueAtAddress addr1 v2
    setValueAtAddress addr2 v1
evaluateSwapWith _ _ = error "Shouldn't happen: wrong types provided"

evaluateAddTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateAddTo v var = binaryModification (+) var v

evaluateMultiplyBy :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateMultiplyBy = binaryModification (*)

evaluateSubtractFrom :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateSubtractFrom v var = binaryModification (-) var v

evaluateDivideBy :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateDivideBy _ (IntV _ 0) = throwHere DivisionByZero
evaluateDivideBy _ (FloatV _ 0) = throwHere DivisionByZero
evaluateDivideBy var v = floatModification (/) var v

evaluateAppendTo :: ReadWrite m => Value a -> Value a -> EvaluatorEnv m ()
evaluateAppendTo v var = listModification (++) var v

--
