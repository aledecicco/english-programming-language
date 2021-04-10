{-# LANGUAGE TupleSections #-}

module Matcher where

import Data.Char ( isUpper, toLower )


import Utils ( getFunId, firstNotNull, allOrNone )
import ParserEnv
import AST

--


-- Auxiliary

-- Returns whether a list of words is a prefix of the given matchable, and the unmatched sufix
isPrefix :: [String] -> [MatchablePart] -> (Bool, [MatchablePart])
isPrefix [] ms = (True, ms)
isPrefix _ [] = (False, [])
isPrefix (t:ts) ms@(WordP w : ps)
    | t == w = isPrefix ts ps
    | otherwise = (False, ms)
isPrefix _ ms = (False, ms)

-- Returns all the ways to split a list in two, with at least one element in the first part
splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = splits' [x] xs
    where
        splits' :: [a] -> [a] -> [([a], [a])]
        splits' l [] = [(l,[])]
        splits' l (r:rs) = (l, r:rs) : splits' (l++[r]) rs

-- Returns all the ways a list of matchables can be used to fill the gaps (parameters) of a title
sepByTitle :: [MatchablePart] -> Title -> [[[MatchablePart]]]
sepByTitle [] [] = [[]]
sepByTitle _ [] = []
sepByTitle ps (TitleWords ws : ts) =
    let (isP, rest) = ws `isPrefix` ps
    in if isP then sepByTitle rest ts else []
sepByTitle ps (TitleParam {} : ts) = do
    (span, rest) <- splits ps
    restMatches <- sepByTitle rest ts
    return $ span:restMatches

--


-- Auxiliary matchers

matchAsName :: [MatchablePart] -> ParserEnv (Maybe Name)
matchAsName [WordP w] = return $ Just [w]
matchAsName (WordP w : ps) = do
    ws <- matchAsName ps
    return $ (w:) <$> ws
matchAsName _ = return Nothing

-- Matches a list of matchables as a call to one of the given functions
matchAsFunctionCall :: [MatchablePart] -> [FunSignature] -> ParserEnv (Maybe (FunId, [Value]))
matchAsFunctionCall ps fs = do
    firstNotNull matchAsFunctionCall' fs
    where
        matchAsFunctionCall' :: FunSignature -> ParserEnv (Maybe (FunId, [Value]))
        matchAsFunctionCall' (FunSignature ft _) = do
            let posParams = sepByTitle ps ft
                fid = getFunId ft
            r <- firstNotNull matchAllParams posParams
            return $ (fid, ) <$> r
        matchAllParams :: [[MatchablePart]] -> ParserEnv (Maybe [Value])
        matchAllParams = allOrNone matchAsValue

--


-- Value matchers

matchAsPrimitive :: [MatchablePart] -> ParserEnv (Maybe Value)
matchAsPrimitive [IntP n] = return $ Just (IntV n)
matchAsPrimitive [FloatP n] = return $ Just (FloatV n)
matchAsPrimitive [CharP c] = return $ Just (CharV c)
matchAsPrimitive [StringP s] = return $ Just (ListV CharT $ map CharV s)
matchAsPrimitive [WordP w]
    | w == "true" = return $ Just (BoolV True)
    | w == "false" = return $ Just (BoolV False)
matchAsPrimitive _ = return Nothing

matchAsVariable :: [MatchablePart] -> ParserEnv (Maybe Value)
matchAsVariable ps = do
    r <- matchAsName ps
    case r of
        Just n -> do
            isDef <- variableIsDefined n
            matchAsVariable' n isDef
        Nothing -> return Nothing
    where
        matchAsVariable' :: Name -> Bool -> ParserEnv (Maybe Value)
        matchAsVariable' n True = return $ Just (VarV n)
        matchAsVariable' ("the":n) False = do
            isDef <- variableIsDefined n
            if isDef
                then return $ Just (VarV n)
                else return Nothing
        matchAsVariable' _ _ = return Nothing

matchAsOperatorCall :: [MatchablePart] -> ParserEnv (Maybe Value)
matchAsOperatorCall ps = do
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall ps operators
    return $ uncurry OperatorCall <$> r

matchAsValue :: [MatchablePart] -> ParserEnv (Maybe Value)
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = firstNotNull (\matcher -> matcher ps) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall]

--


-- Sentence matchers

matchAsProcedureCall :: [MatchablePart] -> ParserEnv (Maybe Sentence)
matchAsProcedureCall ps = do
    procedures <- getProcedureSignatures
    r <- matchAsFunctionCall ps procedures
    matchAsProcedureCall' ps procedures r
    where
        -- Takes the result of matching as a function call, tries again if necessary, and returns a procedure call
        matchAsProcedureCall' :: [MatchablePart] -> [FunSignature] -> Maybe (FunId, [Value]) -> ParserEnv (Maybe Sentence)
        matchAsProcedureCall' (WordP (x:xs) : ps) procedures Nothing
            | isUpper x = do
                let lowerCaseTitle = WordP (toLower x : xs) : ps
                r <- matchAsFunctionCall lowerCaseTitle procedures
                return $ uncurry ProcedureCall <$> r
            | otherwise = return Nothing
        matchAsProcedureCall' _ _ r = return $ uncurry ProcedureCall <$> r


matchAsSentence :: [MatchablePart] -> ParserEnv (Maybe Sentence)
matchAsSentence [ParensP ps] = matchAsSentence ps
matchAsSentence ps = matchAsProcedureCall ps

--
