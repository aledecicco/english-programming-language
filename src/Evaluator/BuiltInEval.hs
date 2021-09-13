{-# LANGUAGE RankNTypes #-}
{-|
Module      : BuiltInEval
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Evaluators for functions in the language's prelude.
-}

module BuiltInEval where

import AST
import Errors (ErrorType(CodeError))
import EvaluatorEnv
import PrettyPrinter (ppValue)


-- -----------------
-- * Auxiliary

-- | Applies an operation to two numeric values and produces a numeric value.
binaryOperation ::
    (forall a. (Num a) => a -> a -> a) -- ^ The operator to use between the numbers contained in the received values.
    -> Bare Value -- ^ The value containing the left operand.
    -> Bare Value -- ^ The value containing the right operand.
    -> Bare Value -- ^ If either operand was a float, the result contains a float. Otherwise, it contains an int.
binaryOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
binaryOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
binaryOperation op (IntV _ n1) (IntV _ n2) = IntV () $ n1 `op` n2
binaryOperation _ a b = error "Shouldn't happen: wrong types provided"

-- | Applies an operation to two int values and produces an int value.
intOperation ::
    (Int -> Int -> Int) -- ^ The operator to use between the numbers contained in the received values.
    -> Bare Value -- ^ The value containing the left operand.
    -> Bare Value -- ^ The value containing the right operand.
    -> Bare Value -- ^ The resulting value containing an int.
intOperation op (IntV _ n1) (IntV _ n2) = IntV () $  n1 `op` n2
intOperation _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Applies an operation to two numeric values and produces a float value.
floatOperation ::
    (Float -> Float -> Float) -- ^ The operator to use between the numbers contained in the received values.
    -> Bare Value -- ^ The value containing the left operand.
    -> Bare Value -- ^ The value containing the right operand.
    -> Bare Value -- ^ The resulting value containing a float.
floatOperation op (IntV _ n) (FloatV _ f) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f) (IntV _ n) = FloatV () $ fromIntegral n `op` f
floatOperation op (FloatV _ f1) (FloatV _ f2) = FloatV () $ f1 `op` f2
floatOperation op (IntV _ n1) (IntV _ n2) = FloatV () $ fromIntegral n1 `op` fromIntegral n2
floatOperation _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Applies an operation to two list values and produces a list value.
-- Assumes the types of both list's elements match.
listOperation ::
    ([Bare Value] -> [Bare Value] -> [Bare Value]) -- ^ The operator to use between the lists of values contained in the received values.
    -> Bare Value -- ^ The value containing the left operand.
    -> Bare Value -- ^ The value containing the right operand.
    -> Bare Value -- ^ The resulting value containing a list of values.
listOperation op (ListV _ t1 vs1) (ListV _ _ vs2) = ListV () t1 $ vs1 `op` vs2
listOperation _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Applies an operation to two numeric values and produces a bool value.
relationalOperation ::
    (forall a. (Ord a) => a -> a -> Bool) -- ^ The operator by which to compare the numbers contained in the received values.
    -> Bare Value -- ^ The value containing the left operand.
    -> Bare Value -- ^ The value containing the right operand.
    -> Bare Value -- ^ The resulting value containing a boolean.
relationalOperation op (IntV _ n) (FloatV _ f) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f) (IntV _ n) = BoolV () $ fromIntegral n `op` f
relationalOperation op (FloatV _ f1) (FloatV _ f2) = BoolV () $ f1 `op` f2
relationalOperation op (IntV _ n1) (IntV _ n2) = BoolV () $ n1 `op` n2
relationalOperation _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Modifies the numeric value pointed at by a reference.
-- Applies a 'binaryOperation' to the referenced value and another numeric value, and changes the referenced value to the result.
binaryModification :: Monad m =>
    (forall a. (Num a) => a -> a -> a) -- ^ The operator to use between the numbers contained in the received values.
    -> Bare Value -- ^ A value containing a reference to a value containing the left operand.
    -> Bare Value -- ^ A value containing the right operand.
    -> EvaluatorEnv m ()
binaryModification op (RefV _ addr) rightN  = do
    leftN <- getValueAtAddress addr
    let newVal = binaryOperation op leftN rightN
    setValueAtAddress addr newVal
binaryModification _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Modifies the float value pointed at by a reference.
-- Applies a 'floatOperation' to the referenced value and another float value, and changes the referenced value to the result.
floatModification :: Monad m =>
    (Float -> Float -> Float) -- ^ The operator to use between the floats contained in the received values.
    -> Bare Value -- ^ A value containing a reference to a value containing the left operand.
    -> Bare Value -- ^ A value containing the right operand.
    -> EvaluatorEnv m ()
floatModification op (RefV _ addr) rightN = do
    leftN <- getValueAtAddress addr
    let newVal = floatOperation op leftN rightN
    setValueAtAddress addr newVal
floatModification _ _ _ = error "Shouldn't happen: wrong types provided"

-- | Modifies the list value pointed at by a reference.
-- Applies a 'listOperation' to the referenced value and another list value, and changes the referenced value to the result.
listModification :: Monad m =>
    ([Bare Value] -> [Bare Value] -> [Bare Value]) -- ^ The operator to use between the lists contained in the received values.
    -> Bare Value -- ^ A value containing a reference to a value containing the left operand.
    -> Bare Value -- ^ A value containing the right operand.
    -> EvaluatorEnv m ()
listModification op (RefV _ addr) rightL = do
    leftL <- getValueAtAddress addr
    let newVal = listOperation op leftL rightL
    setValueAtAddress addr newVal
listModification _ _ _ = error "Shouldn't happen: wrong types provided"


-- -----------------
-- * Operator evaluators

-- | A computation that returns the result of evaluating an operator with the given arguments.
evaluateBuiltInOperator :: Monad m => FunId -> [Bare Value] -> EvaluatorEnv m (Bare Value)
evaluateBuiltInOperator "%_plus_%" [v1, v2] = evaluatePlus v1 v2
evaluateBuiltInOperator "%_times_%" [v1, v2] = evaluateTimes v1 v2
evaluateBuiltInOperator "%_minus_%" [v1, v2] = evaluateMinus v1 v2
evaluateBuiltInOperator "%_divided_by_%" [v1, v2] = evaluateDividedBy v1 v2
evaluateBuiltInOperator "the_quotient_of_%_and_%" [v1, v2] = evaluateTheQuotientOf v1 v2
evaluateBuiltInOperator "%_is_equal_to_%" [v1, v2] = evaluateIsEqualTo v1 v2
evaluateBuiltInOperator "%_is_not_equal_to_%" [v1, v2] = evaluateIsNotEqualTo v1 v2
evaluateBuiltInOperator "%_is_less_than_%" [v1, v2] = evaluateIsLessThan v1 v2
evaluateBuiltInOperator "%_is_less_than_or_equal_to_%" [v1, v2] = evaluateIsLessThanOrEqualTo v1 v2
evaluateBuiltInOperator "%_is_greater_than_%" [v1, v2] = evaluateIsGreaterThan v1 v2
evaluateBuiltInOperator "%_is_greater_than_or_equal_to_%" [v1, v2] = evaluateIsGreaterThanOrEqualTo v1 v2
evaluateBuiltInOperator "the_element_of_%_at_%" [v1, v2] = evaluateElementOfListAt v1 v2
evaluateBuiltInOperator "the_length_of_%" [v] = evaluateLengthOf v
evaluateBuiltInOperator "%_appended_to_%" [v1, v2] = evaluateAppendedTo v1 v2
evaluateBuiltInOperator "the_list_from_%_to_%" [v1, v2] = evaluateTheListFromTo v1 v2
evaluateBuiltInOperator "" _ = error "Shouldn't happen: an operator can't have the empty string as id"
evaluateBuiltInOperator _ _ = error "Shouldn't happen: undefined operator"

-- | Returns the addition of two numeric values.
evaluatePlus :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluatePlus v1 v2 = return $ binaryOperation (+) v1 v2

-- | Returns the multiplication of two numeric values.
evaluateTimes :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateTimes v1 v2 = return $ binaryOperation (*) v1 v2

-- | Returns the subtraction of two numeric values.
evaluateMinus :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateMinus v1 v2 = return $ binaryOperation (-) v1 v2

-- | Returns the division of two numeric values.
evaluateDividedBy :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateDividedBy _ (IntV _ 0) = throwHere $ CodeError ["Division by zero"]
evaluateDividedBy _ (FloatV _ 0) = throwHere $ CodeError ["Division by zero"]
evaluateDividedBy v1 v2 = return $ floatOperation (/) v1 v2

-- | Returns the quotient of two int values.
evaluateTheQuotientOf :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateTheQuotientOf _ (IntV _ 0) = throwHere $ CodeError ["Division by zero"]
evaluateTheQuotientOf v1 v2 = return $ intOperation div v1 v2

-- | Returns whether two values are equal.
evaluateIsEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsEqualTo (IntV _ n) (FloatV _ f) = return $ BoolV () (fromIntegral n == f)
evaluateIsEqualTo (FloatV _ f) (IntV _ n) = return $ BoolV () (fromIntegral n == f)
evaluateIsEqualTo v1 v2 = return $ BoolV () (v1 == v2)

-- | Returns whether two values are different.
evaluateIsNotEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsNotEqualTo (IntV _ n) (FloatV _ f) = return $ BoolV () (fromIntegral n /= f)
evaluateIsNotEqualTo (FloatV _ f) (IntV _ n) = return $ BoolV () (fromIntegral n /= f)
evaluateIsNotEqualTo v1 v2 = return $ BoolV () (v1 /= v2)

-- | Returns whether a numeric value is less than another.
evaluateIsLessThan :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsLessThan v1 v2 = return $ relationalOperation (<) v1 v2

-- | Returns whether a numeric value is less than or equal to another.
evaluateIsLessThanOrEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsLessThanOrEqualTo v1 v2 = return $ relationalOperation (<=) v1 v2

-- | Returns whether a numeric value is greater than another.
evaluateIsGreaterThan :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsGreaterThan v1 v2 = return $ relationalOperation (>) v1 v2

-- | Returns whether a numeric value is greater than or equal to another.
evaluateIsGreaterThanOrEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateIsGreaterThanOrEqualTo v1 v2 = return $ relationalOperation (>=) v1 v2

-- | Returns the nth value in a list value.
evaluateElementOfListAt :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateElementOfListAt (RefV _ addr) (IntV _ index) = do
    r <- getValueAtAddress addr
    case r of
        ListV _ _ vs ->
            if (0 <= index) && (index < length vs)
                then return $ vs !! index
                else throwHere $ CodeError ["Tried to access a list at index", show index, "which is out of bounds"]
        _ -> error "Shouldn't happen: wrong types provided"
evaluateElementOfListAt _ _ = error "Shouldn't happen: wrong types provided"

-- | Returns the length of a list value.
evaluateLengthOf :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
evaluateLengthOf (ListV _ _ vs) = return $ IntV () (length vs)
evaluateLengthOf _ = error "Shouldn't happen: wrong types provided"

-- | Returns the appendment of two list values.
evaluateAppendedTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateAppendedTo v1 v2 = copyValue $ listOperation (++) v1 v2

-- | Returns a list value containing the ints in an inclusive range.
evaluateTheListFromTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateTheListFromTo (IntV _ fromN) (IntV _ toN) = do
    addrs <- mapM addValue [IntV () n | n <- [fromN .. toN]]
    return $ ListV () IntT (map (RefV ()) addrs)
evaluateTheListFromTo _ _ = error "Shouldn't happen: wrong types provided"


-- -----------------
-- * Procedure evaluators

-- | A computation that evaluates a procedure with the given arguments.
evaluateBuiltInProcedure :: ReadWrite m => FunId -> [Bare Value] -> EvaluatorEnv m ()
evaluateBuiltInProcedure "print_%" [v] = evaluatePrint v
evaluateBuiltInProcedure "swap_%_with_%" [v1, v2] = evaluateSwapWith v1 v2
evaluateBuiltInProcedure "add_%_to_%" [v1, v2] = evaluateAddTo v1 v2
evaluateBuiltInProcedure "multiply_%_by_%" [v1, v2] = evaluateMultiplyBy v1 v2
evaluateBuiltInProcedure "subtract_%_from_%" [v1, v2] = evaluateSubtractFrom v1 v2
evaluateBuiltInProcedure "divide_%_by_%" [v1, v2] = evaluateDivideBy v1 v2
evaluateBuiltInProcedure "append_%_to_%" [v1, v2] = evaluateAppendTo v1 v2
evaluateBuiltInProcedure "set_%_to_%" [v1, v2] = evaluateSetTo v1 v2
evaluateBuiltInProcedure "" _ = error "Shouldn't happen: a procedure can't have the empty string as id"
evaluateBuiltInProcedure _ _ = error "Shouldn't happen: undefined procedure"

-- | Pretty-prints a value and writes the result.
evaluatePrint :: ReadWrite m => Bare Value -> EvaluatorEnv m ()
evaluatePrint v = do
    v' <- loadReferences v
    (liftReadWrite . write . ppValue) v'

-- | Swaps the values pointed at by two references with each other.
evaluateSwapWith :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m ()
evaluateSwapWith (RefV _ addr1) (RefV _ addr2) = do
    v1 <- getValueAtAddress addr1
    v2 <- getValueAtAddress addr2
    setValueAtAddress addr1 v2
    setValueAtAddress addr2 v1
evaluateSwapWith _ _ = error "Shouldn't happen: wrong types provided"

-- | Modifies the numeric value pointed at by a reference, adding another numeric value to it.
evaluateAddTo :: Monad m =>
    Bare Value -- ^ The numeric value to add to the referenced value.
    -> Bare Value -- ^ A value containing a reference to a numeric value.
    -> EvaluatorEnv m ()
evaluateAddTo v var = binaryModification (+) var v

-- | Modifies the numeric value pointed at by a reference, multiplying it by another numeric value.
evaluateMultiplyBy :: Monad m =>
    Bare Value -- ^ The numeric value to multiply the referenced value by.
    -> Bare Value -- ^ A value containing a reference to a numeric value.
    -> EvaluatorEnv m ()
evaluateMultiplyBy = binaryModification (*)

-- | Modifies the numeric value pointed at by a reference, subtracting another numeric value from it.
evaluateSubtractFrom :: Monad m =>
    Bare Value -- ^ The numeric value to subtract from the referenced value.
    -> Bare Value -- ^ A value containing a reference to a numeric value.
    -> EvaluatorEnv m ()
evaluateSubtractFrom v var = binaryModification (-) var v

-- | Modifies the numeric value pointed at by a reference, dividing it by another numeric value.
evaluateDivideBy :: Monad m =>
    Bare Value -- ^ A value containing a reference to a numeric value.
    -> Bare Value -- ^ The numeric value to divide the referenced value by.
    -> EvaluatorEnv m ()
evaluateDivideBy _ (IntV _ 0) = throwHere $ CodeError ["Division by zero"]
evaluateDivideBy _ (FloatV _ 0) = throwHere $ CodeError ["Division by zero"]
evaluateDivideBy var v = floatModification (/) var v

-- | Modifies the list value pointed at by a reference, appending another list value to it.
evaluateAppendTo :: Monad m =>
    Bare Value -- ^ The list value to append to the referenced value.
    -> Bare Value -- ^ A value containing a reference to a list value.
    -> EvaluatorEnv m ()
evaluateAppendTo v var = listModification (++) var v

-- | Replaces the value pointed at by a reference with another value.
evaluateSetTo :: Monad m =>
    Bare Value -- ^ A value containing a reference to a value.
    -> Bare Value -- ^ The value to replace the referenced value with.
    -> EvaluatorEnv m ()
evaluateSetTo (RefV _ addr) v = setValueAtAddress addr v
evaluateSetTo _ _ = error "Shouldn't happen: wrong types provided"
