module PrettyPrinter where

import Data.List (intercalate)

import AST
import Errors ( Error(..), ErrorType(..) )

--


-- Auxiliary

surround :: String -> String -> String -> String
surround b a s = b ++ s ++ a

singleQuote :: String -> String
singleQuote = surround "'" "'"

doubleQuote :: String -> String
doubleQuote = surround "\"" "\""

asList :: [String] -> String
asList xs = surround "[" "]" $ intercalate ", " xs

--


-- Pretty printers

ppFunctionId :: FunId -> String
ppFunctionId ('_':cs) = " " ++ ppFunctionId cs
ppFunctionId ('%':cs) = "..." ++ ppFunctionId cs
ppFunctionId (c:cs) = c : ppFunctionId cs
ppFunctionId "" = ""

ppOrdinal :: Int -> String
ppOrdinal p = ppOrdinal' $ show (p+1)
    where
        ppOrdinal' :: String -> String
        ppOrdinal' "1" = "1st"
        ppOrdinal' "2" = "2nd"
        ppOrdinal' "3" = "3rd"
        ppOrdinal' (n:ns) = n : ppOrdinal' ns
        ppOrdinal' [] = "th"

ppType :: Type -> Bool -> String
ppType (AnyT _) False = "thing of any type"
ppType IntT False = "whole number"
ppType FloatT False = "number"
ppType BoolT False = "boolean"
ppType CharT False = "character"
ppType (ListT CharT) False = "string"
ppType (ListT t) False = "list of " ++ ppType t True
ppType (RefT t) False = "reference to a " ++ ppType t False
ppType (AnyT _) True = "things of any type"
ppType IntT True = "whole numbers"
ppType FloatT True = "numbers"
ppType BoolT True = "booleans"
ppType CharT True = "characters"
ppType (ListT CharT) True = "strings"
ppType (ListT t) True = "lists of " ++ ppType t True
ppType (RefT t) True = "references to " ++ ppType t True

ppName :: Name -> String
ppName = unwords

ppValue :: Value a -> String
ppValue (IntV _ n) = show n
ppValue (FloatV _ f) = show f
ppValue (BoolV _ True) = "true"
ppValue (BoolV _ False) = "false"
-- Should chars and strings be quoted?
ppValue (CharV _ c) = [c]
ppValue (ListV _ CharT cs) = concatMap ppValue cs
ppValue (ListV _ _ vs) = asList $ map ppValue vs
ppValue (VarV _ n) = ppName n
ppValue (ValueM _ _) = error "Shouldn't happen: can't print an unsolved value"
ppValue (OperatorCall {}) = error "Shouldn't happen: values must be evaluated before printing them"
ppValue (RefV _ _) = error "Shouldn't happen: references must be solved before printing them"
ppValue (IterV {}) = error "Shouldn't happen: values with iterators must be solved before printing them"

ppMatchablePart :: MatchablePart a -> String
ppMatchablePart (IntP _ n) = show n
ppMatchablePart (FloatP _ f) = show f
ppMatchablePart (CharP _ c) = singleQuote [c]
ppMatchablePart (StringP _ s) = doubleQuote s
ppMatchablePart (WordP _ w) = w
ppMatchablePart (ParensP ps) = surround "(" ")" $ ppMatchable ps

ppMatchable :: [MatchablePart a] -> String
ppMatchable ps = unwords $ map ppMatchablePart ps

ppErrorType :: ErrorType -> String
ppErrorType (WrongTypeValue eT aT) = unwords ["Expected a", ppType eT False, "but got a", ppType aT False, "instead"]
ppErrorType (WrongTypeParameter eT aT n fid) = unwords [ppOrdinal n, "parameter of", doubleQuote $ ppFunctionId fid, "expected a", ppType eT False, "but got a", ppType aT False, "instead"]
ppErrorType (UnmatchableValue ps) = unwords ["Could not understand", doubleQuote $ ppMatchable ps, "as a value"]
ppErrorType (UnmatchableValueTypes ps) = unwords ["Could not understand", doubleQuote $ ppMatchable ps, "as a value because of type errors"]
ppErrorType (UnmatchableSentence ps) = unwords ["Could not understand", doubleQuote $ ppMatchable ps, "as a sentence"]
ppErrorType (UnmatchableSentenceTypes ps) = unwords ["Could not understand", doubleQuote $ ppMatchable ps, "as a sentence because of type errors"]
ppErrorType (FunctionAlreadyDefined fid) = unwords ["Funcion", doubleQuote $ ppFunctionId fid, "is already defined"]
ppErrorType (UndefinedFunction fid) = unwords ["Function", doubleQuote $ ppFunctionId fid, "is not defined"]
ppErrorType (VariableAlreadyDefined n) = unwords ["Expected variable", doubleQuote $ ppName n, "to be new but it was already defined"]
ppErrorType (UndefinedVariable n) = unwords ["Variable", doubleQuote $ ppName n, "is not defined"]
ppErrorType (MismatchingTypeAssigned eT aT n) = unwords ["Could not assign a", ppType aT False, "to variable", doubleQuote $ ppName n, "which is a", ppType eT False]
ppErrorType ResultInProcedure = "Found unexpected result statement in procedure"
ppErrorType ExpectedResult = "Expected a result statement before end of operator"
ppErrorType EmptyList = "Expected a list with at least one element"
ppErrorType (OutOfBoundsIndex i) = unwords ["Tried to access a list at index", show i, "which is out of bounds"]
ppErrorType DivisionByZero = "Division by zero"

ppError :: Error -> String
ppError (Error l eT) =
    let errM = ppErrorType eT
        hM = case l of
            (Just (ln, cn)) -> unwords ["An error occured in line", show ln, "column", show cn]
            Nothing -> "An error occured"
    in hM ++ ":\n" ++ errM
