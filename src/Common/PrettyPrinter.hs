{-|
Module      : PrettyPrinter
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Printing values and errors in a user-friendly way.
-}

module PrettyPrinter where

import Data.Char (isSpace)
import Data.List (intercalate)

import AST
import Errors
import Utils (typeName)

--


-- -----------------
-- * Auxiliary

-- | Surrounds a string with the two given strings.
--
-- >>> surround "(" ")" "a value"
-- "(a value)"
surround ::
    String -- ^ String to place before.
    -> String -- ^ String to place after.
    -> String -- ^ String to surround.
    -> String
surround b a s = b ++ s ++ a

singleQuote :: String -> String
singleQuote = surround "'" "'"

doubleQuote :: String -> String
doubleQuote = surround "\"" "\""

-- | Surrounds a string with angle brackets.
--
-- >>> snippet "a value"
-- "<a value>"
snippet :: String -> String
snippet = surround "<" ">"

-- |
-- >>> asList ["1", "2", "3"]
-- "[1, 2, 3]"
asList :: [String] -> String
asList xs = surround "[" "]" $ intercalate ", " xs


-- -----------------
-- * Pretty-printers

-- |
-- >>> ppFunctionId "%_plus_%"
-- "... plus ..."
ppFunctionId :: FunId -> String
ppFunctionId ('_':cs) = " " ++ ppFunctionId cs
ppFunctionId ('%':cs) = "..." ++ ppFunctionId cs
ppFunctionId (c:cs) = c : ppFunctionId cs
ppFunctionId "" = ""

-- | Transforms an int into an ordinal with the first position being 0.
--
-- >>> ppOrdinal 17
-- "18th"
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
ppValue (CharV _ c) = [c]
ppValue (ListV _ CharT cs) = concatMap ppValue cs
ppValue (ListV _ _ vs) = asList $ map ppValue vs
ppValue (VarV _ n) = ppName n
ppValue (ValueM _ _) = error "Shouldn't happen: can't print an unsolved value"
ppValue (OperatorCall {}) = error "Shouldn't happen: values must be evaluated before printing them"
ppValue (RefV _ _) = error "Shouldn't happen: references must be solved before printing them"
ppValue (IterV {}) = error "Shouldn't happen: values with iterators must be solved before printing them"
ppValue (InputV {}) = error "Shouldn't happen: input values must be solved before printing them"

ppMatchablePart :: MatchablePart a -> String
ppMatchablePart (IntP _ n) = show n
ppMatchablePart (FloatP _ f) = show f
ppMatchablePart (CharP _ c) = singleQuote [c]
ppMatchablePart (StringP _ s) = doubleQuote s
ppMatchablePart (WordP _ w) = w
ppMatchablePart (ParensP ps) = surround "(" ")" $ ppMatchable ps

ppMatchable :: [MatchablePart a] -> String
ppMatchable ps = unwords $ map ppMatchablePart ps

-- | Prints the surroundings of a location in the source code like this:
--
-- @
--    |
-- 17 | Let x be 5 divided by 0.
--    |          ^
-- @
ppSourcePosition :: [String] -> Location -> String
ppSourcePosition lines (lineNum, colNum) =
    let
        -- Generate the prefix for each line depending on the line number.
        strLineNum = show lineNum
        prefix = replicate (length strLineNum) ' ' ++ " | "
        numPrefix = strLineNum ++ " | "
        maxW = width - length prefix

        line = lines !! (lineNum - 1)
        len = length line

        -- Calculate the starting position and length of the snippet to be shown.
        spaces = length $ takeWhile isSpace line
        start = max spaces (min (colNum - pad) (len - maxW))
        lineSpan = take maxW $ drop start line

        -- Add ellipses before or after if the line continues beyond the snippet.
        bef = if start == spaces then "" else "..."
        aft = if start + maxW >= len then "" else "..."
        -- Add an arrow bellow the snippet pointing to the given location.
        pointer = replicate (length bef + colNum - 1 - start) ' ' ++ "^"
    in unlines [prefix, numPrefix ++ surround bef aft lineSpan, prefix ++ pointer]
    where
        width = 74
        pad = 10


-- -----------------
-- * Errors and warnings

ppErrorType :: ErrorType -> String
ppErrorType (WrongTypeValue eT aT) = unwords ["Expected a", ppType eT, "but got a", ppType aT, "instead"]
ppErrorType (WrongTypeArgument eT aT n fid) = unwords [ppOrdinal n, "parameter of", snippet $ ppFunctionId fid, "expected a", ppType eT, "but got a", ppType aT, "instead"]
ppErrorType (UnmatchableValue ps) = unwords ["Couldn't understand", snippet $ ppMatchable ps, "as a value"]
ppErrorType (UnmatchableValueTypes ps) = unwords ["Couldn't understand", snippet $ ppMatchable ps, "as a value because of type errors"]
ppErrorType (UnmatchableSentence ps) = unwords ["Couldn't understand", snippet $ ppMatchable ps, "as a procedure call"]
ppErrorType (UnmatchableSentenceTypes ps) = unwords ["Couldn't understand", snippet $ ppMatchable ps, "as a procedure call because of type errors"]
ppErrorType (UnmatchableInput eT s) = unwords ["Couldn't understand", snippet s, "as a", ppType eT]
ppErrorType (UnreadableType t) = unwords ["The type", ppType t, "is too complex to be directly parsed from input"]
ppErrorType (FunctionAlreadyDefined fid) = unwords ["Funcion", snippet $ ppFunctionId fid, "is already defined"]
ppErrorType (UndefinedFunction fid) = unwords ["Function", snippet $ ppFunctionId fid, "is not defined"]
ppErrorType (VariableAlreadyDefined n) = unwords ["Expected variable", snippet $ ppName n, "to be new but it was already defined"]
ppErrorType (UndefinedVariable n) = unwords ["Variable", snippet $ ppName n, "is not defined"]
ppErrorType (MismatchingTypeAssigned eT aT n) = unwords ["Couldn't assign a", ppType aT, "to variable", snippet $ ppName n, "which is a", ppType eT]
ppErrorType ResultInProcedure = "Found unexpected result statement in procedure"
ppErrorType ExitInOperator  = "Found unexpected exit statement in operator"
ppErrorType ExpectedResult = "Expected a result statement before end of operator"
ppErrorType ForbiddenIteratorUsed = "Can't use iterators here"
ppErrorType BreakOutsideLoop = "Found enexpected break statement outside of a loop"
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
