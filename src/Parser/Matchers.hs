{-# LANGUAGE TupleSections #-}

module Matchers where

import Control.Monad ( void )
import Data.Char ( isUpper, toLower )
import Utils ( allNotNull, getFunId )
import SolverEnv
import AST

--


-- Auxiliary

-- Returns all the ways to split a list in two, with at least one element in the first part
splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = splits' [x] xs
    where
        splits' :: [a] -> [a] -> [([a], [a])]
        splits' l [] = [(l,[])]
        splits' l (r:rs) = (l, r:rs) : splits' (l++[r]) rs

-- Returns whether a list of words is a prefix of the given matchable, and the unmatched sufix
isPrefix :: [String] -> [MatchablePart a] -> (Bool, [MatchablePart a])
isPrefix [] ms = (True, ms)
isPrefix _ [] = (False, [])
isPrefix (t:ts) ms@(WordP _ w : ps)
    | t == w = isPrefix ts ps
    | otherwise = (False, ms)
isPrefix _ ms = (False, ms)

-- Returns all the ways a list of matchables can be used to fill the gaps (parameters) of a title
sepByTitle :: [MatchablePart a] -> [TitlePart b] -> [[[MatchablePart a]]]
sepByTitle [] [] = [[]]
sepByTitle _ [] = []
sepByTitle ps (TitleWords _ ws : ts) =
    let (isP, rest) = ws `isPrefix` ps
    in if isP then sepByTitle rest ts else []
sepByTitle ps (TitleParam {} : ts) = do
    (span, rest) <- splits ps
    restMatches <- sepByTitle rest ts
    return $ span:restMatches

-- Returns the matchables in a list before and after a given matchable
splitBy :: [MatchablePart a] -> MatchablePart b -> ([MatchablePart a], [MatchablePart a])
splitBy [] _ = ([], [])
splitBy (p:ps) p' =
    if void p == void p'
        then ([], ps)
        else
            let (bef, aft) = splitBy ps p'
            in (p:bef, aft)

--


-- Auxiliary matchers

matchAsName :: [MatchablePart a] -> SolverEnv (Maybe Name)
matchAsName [WordP _ w] = return $ Just [w]
matchAsName (WordP _ w : ps) = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ Just (w : ws)
        Nothing -> return Nothing
matchAsName _ = return Nothing

matchAsType :: [MatchablePart a] -> SolverEnv (Maybe Type)
matchAsType ps = do
    r <- matchAsName ps
    case r of
        Just ws -> return $ matchAsType' ws False
        Nothing -> return Nothing
    where
        matchAsType' :: Name -> Bool -> Maybe Type
        matchAsType' ["whole", "number"] False = Just IntT
        matchAsType' ["number"] False = Just FloatT
        matchAsType' ["boolean"] False = Just BoolT
        matchAsType' ["character"] False = Just CharT
        matchAsType' ["string"] False = Just $ ListT CharT
        matchAsType' ("list":"of":ws) False = ListT <$> matchAsType' ws True
        matchAsType' ["whole", "numbers"] True = Just IntT
        matchAsType' ["numbers"] True = Just FloatT
        matchAsType' ["booleans"] True = Just BoolT
        matchAsType' ["characters"] True = Just CharT
        matchAsType' ["strings"] True = Just $ ListT CharT
        matchAsType' ("lists":"of":ws) True = ListT <$> matchAsType' ws True
        matchAsType' _ _ = Nothing

matchAsFunctionCall :: [Annotated MatchablePart] -> [FunSignature] -> SolverEnv [(FunId, [Annotated Value])]
matchAsFunctionCall ps fs = concat <$> mapM (matchAsFunctionCall' ps) fs
    where
        matchAsFunctionCall' :: [Annotated MatchablePart] -> FunSignature -> SolverEnv [(FunId, [Annotated Value])]
        matchAsFunctionCall' ps (FunSignature (Title _ ft) _) = do
            let posParams = sepByTitle ps ft
                fid = getFunId ft
            paramMatches <- allNotNull matchAllParams posParams
            let paramCombs = map sequence paramMatches
            return $ concatMap (map (fid,)) paramCombs

        matchAllParams :: [[Annotated MatchablePart]] -> SolverEnv (Maybe [[Annotated Value]])
        matchAllParams pss = do
            r <- mapM matchAsValue pss
            if any null r
                then return Nothing
                else return $ Just r

--


-- Value matchers

matchAsPrimitive :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsPrimitive [IntP ann n] = return [IntV ann n]
matchAsPrimitive [FloatP ann n] = return [FloatV ann n]
matchAsPrimitive [CharP ann c] = return [CharV ann c]
matchAsPrimitive [StringP ann@(ln, cn) s] =
    let cns = [cn+1 ..]
        lvs = zip s cns
        locateC = \(c, cn') -> CharV (ln, cn') c
    in return [ListV ann CharT $ map locateC lvs]
matchAsPrimitive [WordP ann w]
    | w == "true" = return [BoolV ann True]
    | w == "false" = return [BoolV ann False]
matchAsPrimitive _ = return []

matchAsVariable :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsVariable ps = do
    r <- matchAsName ps
    let posN = case r of
            Just n@("the":w:ws) -> if w == "the" then [n] else [n, w:ws]
            Just n -> [n]
            Nothing -> []
        ann = getFirstLocation ps
    ns <- allNotNull matchAsVariable' posN
    return $ map (VarV ann) ns
    where
        matchAsVariable' :: Name -> SolverEnv (Maybe Name)
        matchAsVariable' n = do
            isDef <- variableIsDefined n
            if isDef
                then return $ Just n
                else return Nothing

matchAsOperatorCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsOperatorCall ps = do
    let ann = getFirstLocation ps
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall ps operators
    return $ map (uncurry $ OperatorCall ann) r

matchAsIterator :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsIterator (WordP ann "each":ps) = do
    let (bef, aft) = splitBy ps (WordP () "in")
    r <- matchAsType bef
    case r of
        Just t -> do
            r <- matchAsValue aft
            return $ map (IterV ann t) r
        Nothing -> return []
matchAsIterator _ = return []

matchAsValue :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsValue [ParensP ps] = matchAsValue ps
matchAsValue ps = do
    r <- mapM (\matcher -> matcher ps) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall, matchAsIterator]
    return $ concat r

--


-- Sentence matchers

matchAsProcedureCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsProcedureCall ps = do
    procedures <- getProcedureSignatures
    r <- matchAsFunctionCall ps procedures
    r' <- case ps of
        (WordP fAnn (x:xs) : ps) ->
            if isUpper x
                then do
                    let lowerCaseTitle = WordP fAnn (toLower x : xs) : ps
                    matchAsFunctionCall lowerCaseTitle procedures
                else return []
        _ -> return []
    let ann = getFirstLocation ps
    return $ map (uncurry  $ ProcedureCall ann) (r ++ r')

matchAsSentence :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsSentence [ParensP ps] = matchAsSentence ps
matchAsSentence ps = matchAsProcedureCall ps

--
