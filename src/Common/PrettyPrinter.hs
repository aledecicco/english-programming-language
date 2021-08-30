module PrettyPrinter where

import Data.Bifunctor ( first, second )
import Data.Char ( isSpace )
import Data.List ( intercalate )
import Control.Monad ( void, when )
import Control.Monad.Trans.State ( get, modify, runState, State )

import AST
import Errors ( Error(..), ErrorType(..), Warning (..), WarningType (..) )
import Utils ( typeName )

--


-- Auxiliary

surround :: String -> String -> String -> String
surround b a s = b ++ s ++ a

singleQuote :: String -> String
singleQuote = surround "'" "'"

doubleQuote :: String -> String
doubleQuote = surround "\"" "\""

snippet :: String -> String
snippet = surround "<" ">"

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

ppType :: Type -> String
ppType t = ppName $ typeName t False

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
ppValue v@(OperatorCall {}) = error "Shouldn't happen: values must be evaluated before printing them"
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

ppSourcePosition :: [String] -> Location -> String
ppSourcePosition ls (ln, cn) =
    let
        ln' = show ln
        sep = replicate (length ln') ' ' ++ " | "
        maxW = width - length sep

        l = ls !! (ln - 1)
        lenL = length l

        spaces = length $ takeWhile isSpace l
        start = max spaces (min (cn - pad) (lenL - maxW))
        l' = take maxW $ drop start l

        bef = if start == spaces then "" else "..."
        aft = if start + maxW >= lenL then "" else "..."
        pntr = replicate (length bef + cn - 1 - start) ' ' ++ "^"
    in unlines [sep, ln' ++ " | " ++ bef ++ l' ++ aft, sep ++ pntr]
    where
        width = 74
        pad = 10

--


-- Errors and warnings

ppErrorType :: ErrorType -> String
ppErrorType (WrongTypeValue eT aT) = unwords ["Expected a", ppType eT, "but got a", ppType aT, "instead"]
ppErrorType (WrongTypeParameter eT aT n fid) = unwords [ppOrdinal n, "parameter of", snippet $ ppFunctionId fid, "expected a", ppType eT, "but got a", ppType aT, "instead"]
ppErrorType (UnmatchableValue ps) = unwords ["Could not understand", snippet $ ppMatchable ps, "as a value"]
ppErrorType (UnmatchableValueTypes ps) = unwords ["Could not understand", snippet $ ppMatchable ps, "as a value because of type errors"]
ppErrorType (UnmatchableSentence ps) = unwords ["Could not understand", snippet $ ppMatchable ps, "as a procedure call"]
ppErrorType (UnmatchableSentenceTypes ps) = unwords ["Could not understand", snippet $ ppMatchable ps, "as a procedure call because of type errors"]
ppErrorType (FunctionAlreadyDefined fid) = unwords ["Funcion", snippet $ ppFunctionId fid, "is already defined"]
ppErrorType (UndefinedFunction fid) = unwords ["Function", snippet $ ppFunctionId fid, "is not defined"]
ppErrorType (VariableAlreadyDefined n) = unwords ["Expected variable", snippet $ ppName n, "to be new but it was already defined"]
ppErrorType (UndefinedVariable n) = unwords ["Variable", snippet $ ppName n, "is not defined"]
ppErrorType (MismatchingTypeAssigned eT aT n) = unwords ["Could not assign a", ppType aT, "to variable", snippet $ ppName n, "which is a", ppType eT]
ppErrorType ResultInProcedure = "Found unexpected result statement in procedure"
ppErrorType ExpectedResult = "Expected a result statement before end of operator"
ppErrorType ForbiddenIteratorUsed = "Can't use iterators here"
ppErrorType (CodeError s) = unwords s
ppErrorType (ParseError s) = s

ppError :: [String] -> Error -> String
ppError ls (Error l eT) =
    let errM = ppErrorType eT
        errH = case l of
            (Just (ln, cn)) -> unwords ["Error in line", show ln, "column", show cn] ++ ":\n" ++ ppSourcePosition ls (ln, cn)
            Nothing -> "Error:\n"
    in errH ++ errM ++ "."

ppWarningType :: WarningType -> String
ppWarningType (AmbiguousValue i ps) = unwords ["Value", snippet $ ppMatchable ps, "can be understood in", show i, "different ways"]
ppWarningType (AmbiguousSentence i ps) = unwords ["Sentence", snippet $ ppMatchable ps, "can be understood in", show i, "different ways"]

ppWarning :: [String] -> Warning -> String
ppWarning ls (Warning l wT) =
    let wrnM = ppWarningType wT
        wrnH = case l of
            (Just (ln, cn)) -> unwords ["Warning in line", show ln, "column", show cn] ++ ":\n" ++ ppSourcePosition ls (ln, cn)
            Nothing -> "Warning:\n"
    in wrnH ++ wrnM ++ "."

--

