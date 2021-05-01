{-# LANGUAGE TupleSections #-}

module Matcher where

import Data.Char ( isUpper, toLower )


import Utils ( getFunId, firstNotNull, allOrNone )
import ParserEnv
import AST

--


-- Auxiliary

-- Returns whether a list of words is a prefix of the given matchable, and the unmatched sufix
isPrefix :: [String] -> [Annotated MatchablePart] -> (Bool, [Annotated MatchablePart])
isPrefix [] ms = (True, ms)
isPrefix _ [] = (False, [])
isPrefix (t:ts) ms@(WordP _ w : ps)
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
sepByTitle :: [Annotated MatchablePart] -> [TitlePart a] -> [[[Annotated MatchablePart]]]
sepByTitle [] [] = [[]]
sepByTitle _ [] = []
sepByTitle ps (TitleWords _ ws : ts) =
    let (isP, rest) = ws `isPrefix` ps
    in if isP then sepByTitle rest ts else []
sepByTitle ps (TitleParam {} : ts) = do
    (span, rest) <- splits ps
    restMatches <- sepByTitle rest ts
    return $ span:restMatches

--


-- Auxiliary matchers

matchAsName :: [Annotated MatchablePart] -> ParserEnv (Maybe Name)
matchAsName [WordP _ w] = return $ Just [w]
matchAsName (WordP _ w : ps) = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ Just (w : ws)
        Nothing -> return Nothing
matchAsName _ = return Nothing

-- Matches a list of matchables as a call to one of the given functions
matchAsFunctionCall :: [Annotated MatchablePart] -> [FunSignature] -> ParserEnv (Maybe (FunId, [Annotated Value]))
matchAsFunctionCall ps fs = do
    firstNotNull matchAsFunctionCall' fs
    where
        matchAsFunctionCall' :: FunSignature -> ParserEnv (Maybe (FunId, [Annotated Value]))
        matchAsFunctionCall' (FunSignature (Title _ ft) _) = do
            let posParams = sepByTitle ps ft
                fid = getFunId ft
            r <- firstNotNull matchAllParams posParams
            return $ (fid, ) <$> r
        matchAllParams :: [[Annotated MatchablePart]] -> ParserEnv (Maybe [Annotated Value])
        matchAllParams = allOrNone matchAsValue

--


-- Value matchers

matchAsPrimitive :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsPrimitive [IntP ann n] = return $ Just (IntV ann n)
matchAsPrimitive [FloatP ann n] = return $ Just (FloatV ann n)
matchAsPrimitive [CharP ann c] = return $ Just (CharV ann c)
matchAsPrimitive [StringP ann@(ln, cn) s] =
    let cns = [cn+1 ..]
        lvs = zip s cns
        locateC = \(c, cn') -> CharV (ln, cn') c
    in return $ Just (ListV ann CharT $ map locateC lvs)
matchAsPrimitive [WordP ann w]
    | w == "true" = return $ Just (BoolV ann True)
    | w == "false" = return $ Just (BoolV ann False)
matchAsPrimitive _ = return Nothing

-- ToDo: refactor
matchAsVariable :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsVariable ps = do
    let ann = getFirstAnnotation ps
    r <- matchAsName ps
    case r of
        Just n -> do
            isDef <- variableIsDefined n
            matchAsVariable' n isDef ann
        Nothing -> return Nothing
    where
        matchAsVariable' :: Name -> Bool -> Location -> ParserEnv (Maybe (Annotated Value))
        matchAsVariable' n True ann = return $ Just (VarV ann n)
        matchAsVariable' ("the":n) False ann = do
            isDef <- variableIsDefined n
            if isDef
                then return $ Just (VarV ann n)
                else return Nothing
        matchAsVariable' _ _ _ = return Nothing

matchAsOperatorCall :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsOperatorCall ps = do
    let ann = getFirstAnnotation ps
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall ps operators
    return $ uncurry (OperatorCall ann) <$> r

matchAsValue :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Value))
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = firstNotNull (\matcher -> matcher ps) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall]

--


-- Sentence matchers

matchAsProcedureCall :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Sentence))
matchAsProcedureCall ps = do
    procedures <- getProcedureSignatures
    r <- matchAsFunctionCall ps procedures
    matchAsProcedureCall' ps procedures r
    where
        -- Takes the result of matching as a function call, tries again if necessary, and returns a procedure call
        matchAsProcedureCall' :: [Annotated MatchablePart] -> [FunSignature] -> Maybe (FunId, [Annotated Value]) -> ParserEnv (Maybe (Annotated Sentence))
        matchAsProcedureCall' (WordP fAnn (x:xs) : ps) procedures Nothing
            | isUpper x = do
                let lowerCaseTitle = WordP fAnn (toLower x : xs) : ps
                r <- matchAsFunctionCall lowerCaseTitle procedures
                return $ uncurry (ProcedureCall fAnn) <$> r
            | otherwise = return Nothing
        matchAsProcedureCall' ps _ r = do
            let ann = getFirstAnnotation ps
            return $ uncurry (ProcedureCall ann) <$> r


matchAsSentence :: [Annotated MatchablePart] -> ParserEnv (Maybe (Annotated Sentence))
matchAsSentence [ParensP ps] = matchAsSentence ps
matchAsSentence ps = matchAsProcedureCall ps

--
