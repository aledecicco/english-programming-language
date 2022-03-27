{-# LANGUAGE RankNTypes #-}
{-|
Module      : BuiltInEval
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Evaluators for functions in the language's prelude.
-}

module BuiltInEval2 where

import AST
import Control.Monad (zipWithM, unless)
import Data.Char (toUpper, toLower)
import EvaluatorEnv
import Errors (ErrorType(CodeError))


-- -----------------
-- * Auxiliary

modifyRef :: Monad m => (Bare Value -> Bare Value) -> Bare Value -> EvaluatorEnv m ()
modifyRef op (RefV _ addr) = do
    val <- getValueAtAddress addr
    let newVal = op val
    setValueAtAddress addr newVal
modifyRef _ _ = error "Shouldn't happen: wrong types provided"

charUnaryOp :: (Char -> Char) -> Bare Value -> Char
charUnaryOp op (CharV _ c) = op c
charUnaryOp _ _ = error "Shouldn't happen: wrong types provided"

boolUnaryOp :: (Bool -> Bool) -> Bare Value -> Bool
boolUnaryOp op (BoolV _ b) = op b
boolUnaryOp _ _ = error "Shouldn't happen: wrong types provided"

boolBinaryOp :: (Bool -> Bool -> Bool) -> Bare Value -> Bare Value -> Bool
boolBinaryOp op (BoolV _ b1) (BoolV _ b2) = op b1 b2
boolBinaryOp _ _ _ = error "Shouldn't happen: wrong types provided"

listUnaryMod :: ([Bare Value] -> [Bare Value]) -> Bare Value -> Bare Value
listUnaryMod op (ListV _ elemsType elems) = ListV () elemsType $ op elems
listUnaryMod _ _ = error "Shouldn't happen: wrong types provided"

infoFromList :: ([Bare Value] -> a) -> Bare Value -> a
infoFromList op (ListV _ _ elems) = op elems
infoFromList _ _ = error "Shouldn't happen: wrong types provided"

checkListIndex :: Monad m => Int -> Int -> EvaluatorEnv m ()
checkListIndex len n = do
    let isInRange = (0 <= n) && (n < len)
    unless isInRange $ throwHere (CodeError ["Tried to access a list at index", show n, "which is out of bounds"])

checkListNotEmpty :: Monad m => Bare Value -> EvaluatorEnv m ()
checkListNotEmpty listVal = do
    len <- evaluateLength listVal
    unless (len > 0) $ throwHere (CodeError ["Tried to access an element of an empty list"])


-- -----------------
-- * Operator evaluators

-- | A computation that returns the result of evaluating an operator with the given arguments.
evaluateBuiltInOperator :: Monad m => FunId -> [Bare Value] -> EvaluatorEnv m (Bare Value)
evaluateBuiltInOperator "%_in_uppercase" [charVal] = CharV () <$> evaluateInUppercase charVal
evaluateBuiltInOperator "%_in_lowercase" [charVal] = CharV () <$> evaluateInLowercase charVal
evaluateBuiltInOperator "%_is_true" [boolVal] = BoolV () <$> evaluateIsTrue boolVal
evaluateBuiltInOperator "%_is_false" [boolVal] = BoolV () <$> evaluateIsFalse boolVal
evaluateBuiltInOperator "whether_%" [boolVal] = BoolV () <$> evaluateWhether boolVal
evaluateBuiltInOperator "not_%" [boolVal] = BoolV () <$> evaluateNot boolVal
evaluateBuiltInOperator "%_negated" [boolVal] = BoolV () <$> evaluateNegated boolVal
evaluateBuiltInOperator "%_and_%" [boolVal1, boolVal2] = BoolV () <$> evaluateAnd boolVal1 boolVal2
evaluateBuiltInOperator "%_or_%" [boolVal1, boolVal2] = BoolV () <$> evaluateOr boolVal1 boolVal2
evaluateBuiltInOperator "%_or_%_but_not_both" [boolVal1, boolVal2] = BoolV () <$> evaluateXor boolVal1 boolVal2
evaluateBuiltInOperator "the_length_of_%" [listVal] = IntV () <$> evaluateLength listVal
evaluateBuiltInOperator "%_is_empty" [listVal] = BoolV () <$> evaluateIsEmpty listVal
evaluateBuiltInOperator "%_is_not_empty" [listVal] = BoolV () <$> evaluateIsNotEmpty listVal
evaluateBuiltInOperator "%_contains_%" [listVal, elemVal] = BoolV () <$> evaluateContains listVal elemVal
evaluateBuiltInOperator "the_element_of_%_at_%" [listVal, nVal] = evaluateNthElement listVal nVal
evaluateBuiltInOperator "%_without_the_element_at_%" [listVal, nVal] = evaluateWithoutNth listVal nVal
evaluateBuiltInOperator "%_without_the_first_apparition_of_%" [listVal, elemVal] = evaluateWithoutFirstApparition listVal elemVal
evaluateBuiltInOperator "%_without_all_apparitions_of_%" [listVal, elemVal] = evaluateWithoutAllApparitions listVal elemVal
evaluateBuiltInOperator "%_without_its_first_element" [listVal] = evaluateWithoutFirst listVal
evaluateBuiltInOperator "%_without_its_last_element" [listVal] = evaluateWithoutLast listVal
evaluateBuiltInOperator "%_with_%_added_at_the_beggining" [listVal, elemVal] = evaluateWithAddedAtBeggining listVal elemVal
evaluateBuiltInOperator "%_with_%_added_at_the_end" [listVal, elemVal] = evaluateWithAddedAtEnd listVal elemVal
evaluateBuiltInOperator "%_with_%_added_at_%" [listVal, elemVal, nVal] = evaluateWithAddedAtN listVal elemVal nVal
evaluateBuiltInOperator "%_is_equal_to_%" [v1, v2] = BoolV () <$> evaluateIsEqualTo v1 v2
evaluateBuiltInOperator "%_is_not_equal_to_%" [v1, v2] = BoolV () <$> evaluateIsNotEqualTo v1 v2
evaluateBuiltInOperator "" _ = error "Shouldn't happen: an operator can't have the empty string as id"
evaluateBuiltInOperator _ _ = error "Shouldn't happen: undefined operator"

evaluateInUppercase :: Monad m => Bare Value -> EvaluatorEnv m Char
evaluateInUppercase charVal = return $ charUnaryOp toUpper charVal

evaluateInLowercase :: Monad m => Bare Value -> EvaluatorEnv m Char
evaluateInLowercase charVal = return $ charUnaryOp toLower charVal

evaluateIsTrue :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateIsTrue boolVal = return $ boolUnaryOp id boolVal

evaluateIsFalse :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateIsFalse boolVal = not <$> evaluateIsTrue boolVal

evaluateWhether :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateWhether = evaluateIsTrue

evaluateNot :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateNot = evaluateIsFalse

evaluateAnd :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateAnd boolVal1 boolVal2 = return $ boolBinaryOp (&&) boolVal1 boolVal2

evaluateOr :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateOr boolVal1 boolVal2 = return $ boolBinaryOp (||) boolVal1 boolVal2

evaluateXor :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateXor boolVal1 boolVal2 = return $ boolBinaryOp (/=) boolVal1 boolVal2

evaluateNegated :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateNegated = evaluateIsFalse

evaluateLength :: Monad m => Bare Value -> EvaluatorEnv m Int
evaluateLength listVal = return $ infoFromList length listVal

evaluateIsEmpty :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateIsEmpty listVal = return $ infoFromList null listVal

evaluateIsNotEmpty :: Monad m => Bare Value -> EvaluatorEnv m Bool
evaluateIsNotEmpty listVal = not <$> evaluateIsEmpty listVal

evaluateContains :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateContains listVal elemVal = do
    equalities <- infoFromList (mapM $ evaluateIsEqualTo elemVal) listVal
    return $ or equalities

evaluateNthElement :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateNthElement listVal (IntV _ n) = do
    len <- evaluateLength listVal
    checkListIndex len n
    return $ infoFromList (!! n) listVal
evaluateNthElement _ _ = error "Shouldn't happen: wrong types provided"

evaluateWithoutNth :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithoutNth listVal (IntV _ n) = do
    len <- evaluateLength listVal
    checkListIndex len n
    return $ listUnaryMod (\elems -> take n elems ++ drop (n+1) elems) listVal
evaluateWithoutNth _ _ = error "Shouldn't happen: wrong types provided"

evaluateWithoutFirstApparition :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithoutFirstApparition (ListV _ elemsType elems) elemVal = ListV () elemsType <$> withoutFirst elems elemVal
    where
        withoutFirst :: Monad m => [Bare Value] -> Bare Value -> EvaluatorEnv m [Bare Value]
        withoutFirst [] _ = return []
        withoutFirst (elem:elems) elemVal = do
            isEqual <- evaluateIsEqualTo elem elemVal
            if isEqual
                then return elems
                else (elem:) <$> withoutFirst elems elemVal
evaluateWithoutFirstApparition _ _ = error "Shouldn't happen: wrong types provided"

evaluateWithoutAllApparitions :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithoutAllApparitions (ListV _ elemsType elems) elemVal = ListV () elemsType <$> withoutAll elems elemVal
    where
        withoutAll :: Monad m => [Bare Value] -> Bare Value -> EvaluatorEnv m [Bare Value]
        withoutAll [] _ = return []
        withoutAll (elem:elems) elemVal = do
            isEqual <- evaluateIsEqualTo elem elemVal
            if isEqual
                then withoutAll elems elemVal
                else (elem:) <$> withoutAll elems elemVal
evaluateWithoutAllApparitions _ _ = error "Shouldn't happen: wrong types provided"

evaluateWithoutFirst :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithoutFirst listVal = do
    checkListNotEmpty listVal
    return $ listUnaryMod tail listVal

evaluateWithoutLast :: Monad m => Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithoutLast listVal = do
    checkListNotEmpty listVal
    return $ listUnaryMod init listVal

evaluateWithAddedAtBeggining :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithAddedAtBeggining listVal elemVal = do
    addr <- addValue elemVal
    return $ listUnaryMod (RefV () addr :) listVal

evaluateWithAddedAtEnd :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithAddedAtEnd listVal elemVal = do
    addr <- addValue elemVal
    return $ listUnaryMod (++ [RefV () addr]) listVal

evaluateWithAddedAtN :: Monad m => Bare Value -> Bare Value -> Bare Value -> EvaluatorEnv m (Bare Value)
evaluateWithAddedAtN listVal elemVal (IntV _ n) = do
    len <- evaluateLength listVal
    checkListIndex (len+1) n
    addr <- addValue elemVal
    return $ listUnaryMod (\elems -> take n elems ++ [RefV () addr] ++ drop n elems) listVal
evaluateWithAddedAtN _ _ _ = error "Shouldn't happen: wrong types provided"

evaluateIsEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateIsEqualTo (RefV _ addr) v2 = do
    v1 <- getValueAtAddress addr
    evaluateIsEqualTo v1 v2
evaluateIsEqualTo v1 (RefV _ addr) = do
    v2 <- getValueAtAddress addr
    evaluateIsEqualTo v1 v2
evaluateIsEqualTo (IntV _ n) (FloatV _ f) = return $ fromIntegral n == f
evaluateIsEqualTo (FloatV _ f) (IntV _ n) = return $ f == fromIntegral n
evaluateIsEqualTo (ListV _ _ e1) (ListV _ _ e2) =
    if length e1 /= length e2
        then return False
        else do
            results <- zipWithM evaluateIsEqualTo e1 e2
            return $ and results
evaluateIsEqualTo v1 v2 = return $ v1 == v2

evaluateIsNotEqualTo :: Monad m => Bare Value -> Bare Value -> EvaluatorEnv m Bool
evaluateIsNotEqualTo v1 v2 = not <$> evaluateIsEqualTo v1 v2


-- -----------------
-- * Procedure evaluators

-- | A computation that evaluates a procedure with the given arguments.
evaluateBuiltInProcedure :: ReadWrite m => FunId -> [Bare Value] -> EvaluatorEnv m ()
evaluateBuiltInProcedure "transform_%_to_uppercase" [charVal] = evaluateToUppercase charVal
evaluateBuiltInProcedure "transform_%_to_lowercase" [charVal] = evaluateToLowercase charVal
evaluateBuiltInProcedure "negate_%" [boolVal] = evaluateNegate boolVal
evaluateBuiltInProcedure "" _ = error "Shouldn't happen: a procedure can't have the empty string as id"
evaluateBuiltInProcedure _ _ = error "Shouldn't happen: undefined procedure"

evaluateToUppercase :: Monad m => Bare Value -> EvaluatorEnv m ()
evaluateToUppercase = modifyRef $ CharV () . charUnaryOp toUpper

evaluateToLowercase :: Monad m => Bare Value -> EvaluatorEnv m ()
evaluateToLowercase = modifyRef $ CharV () . charUnaryOp toLower

evaluateNegate :: Monad m => Bare Value -> EvaluatorEnv m ()
evaluateNegate = modifyRef $ BoolV () . boolUnaryOp not
