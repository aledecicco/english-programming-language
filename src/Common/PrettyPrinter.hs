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

ppValue :: Value -> String
ppValue (IntV n) = show n
ppValue (FloatV f) = show f
ppValue (BoolV True) = "true"
ppValue (BoolV False) = "false"
ppValue (CharV c) = singleQuote [c]
ppValue (ListV CharT cs) = doubleQuote $ map getChar cs
    where
        getChar :: Value -> Char
        getChar (CharV c) = c
ppValue (ListV _ vs) = asList $ map ppValue vs
ppValue (VarV n) = ppName n

ppMatchablePart :: MatchablePart -> String
ppMatchablePart (IntP n) = show n
ppMatchablePart (FloatP f) = show f
ppMatchablePart (CharP c) = singleQuote [c]
ppMatchablePart (StringP s) = doubleQuote s
ppMatchablePart (WordP w) = w
ppMatchablePart (ParensP ps) = surround "(" ")" $ ppMatchable ps

ppMatchable :: [MatchablePart] -> String
ppMatchable ps = unwords $ map ppMatchablePart ps
