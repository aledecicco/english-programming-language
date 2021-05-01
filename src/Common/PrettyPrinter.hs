module PrettyPrinter where

import Data.List (intercalate)

import AST

--


-- Auxiliary

surround :: String -> String -> String -> String
surround b a s = b ++ s ++ a

singleQuote :: String -> String
singleQuote = surround "'" "'"

doubleQuote :: String -> String
doubleQuote = surround "\"" "\""

asList :: [String] -> String
asList xs = surround "[" "]" $ intercalate "," xs


--


-- Pretty printers

ppFunctionId :: FunId -> String
ppFunctionId ('_':cs) = " " ++ ppFunctionId cs
ppFunctionId ('%':cs) = "..." ++ ppFunctionId cs
ppFunctionId (c:cs) = c : ppFunctionId cs

ppType :: Type -> Bool -> String
ppType (AnyT a) _ = "any " ++ doubleQuote a
ppType IntT False = "whole number"
ppType FloatT False = "number"
ppType BoolT False = "boolean"
ppType CharT False = "character"
ppType (ListT CharT) False = "string"
ppType (ListT t) False = "list of " ++ ppType t True
ppType IntT True = "whole numbers"
ppType FloatT True = "numbers"
ppType BoolT True = "booleans"
ppType CharT True = "characters"
ppType (ListT CharT) True = "strings"
ppType (ListT t) True = "lists of " ++ ppType t True

ppName :: Name -> String
ppName = unwords

ppValue :: Value a -> String
ppValue (IntV _ n) = show n
ppValue (FloatV _ f) = show f
ppValue (BoolV _ True) = "true"
ppValue (BoolV _ False) = "false"
ppValue (CharV _ c) = singleQuote [c]
ppValue (ListV _ CharT cs) = doubleQuote $ map getChar cs
    where
        getChar :: Value a -> Char
        getChar (CharV _ c) = c
ppValue (ListV _ _ vs) = asList $ map ppValue vs
ppValue (VarV _ n) = ppName n

ppMatchablePart :: MatchablePart a -> String
ppMatchablePart (IntP _ n) = show n
ppMatchablePart (FloatP _ f) = show f
ppMatchablePart (CharP _ c) = singleQuote [c]
ppMatchablePart (StringP _ s) = doubleQuote s
ppMatchablePart (WordP _ w) = w
ppMatchablePart (ParensP ps) = surround "(" ")" $ ppMatchable ps

ppMatchable :: [MatchablePart a] -> String
ppMatchable ps = unwords $ map ppMatchablePart ps

ppError :: [String] -> Location -> String
ppError e (ln, cn) = unwords ["Error in line", show ln, "column", show cn, ":\n", unwords e, "\n"]
