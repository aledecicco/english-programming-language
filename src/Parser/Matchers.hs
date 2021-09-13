{-# LANGUAGE TupleSections #-}
{-|
Module      : Matchers
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Functions used by the "Solver" to turn MatchableParts into Values and Sentences.
-}

module Matchers where

import Control.Monad (filterM, void)
import Data.Char (isUpper, toLower)

import AST
import SolverEnv
import Utils (allNotNull, getFunId)


-- -----------------
-- * Auxiliary

-- | Returns all the ways to split a list in two with at least one element in the first part.
--
-- >>> splits [1, 2, 3]
-- [([1],[2,3]),([1,2],[3]),([1,2,3],[])]
splits :: [a] -> [([a], [a])]
splits [] = []
splits (x:xs) = splits' [x] xs
    where
        splits' :: [a] -> [a] -> [([a], [a])]
        splits' l [] = [(l,[])]
        splits' l (r:rs) = (l, r:rs) : splits' (l++[r]) rs

-- | Returns whether a list of words is a prefix of the given matchable, and the unmatched sufix.
isPrefix :: [String] -> [MatchablePart a] -> (Bool, [MatchablePart a])
isPrefix [] parts = (True, parts)
isPrefix _ [] = (False, [])
isPrefix (w:ws) parts@(WordP _ w' : ws')
    | w == w' = isPrefix ws ws'
    | otherwise = (False, parts)
isPrefix _ parts = (False, parts)

-- | Returns all the ways a list of matchables can be used to fill the gaps (parameters) of a title.
sepByTitle :: [MatchablePart a] -> [TitlePart b] -> [[[MatchablePart a]]]
sepByTitle [] [] = [[]]
sepByTitle _ [] = []
sepByTitle parts (TitleWords _ ws : titleParts) =
    let (isP, rest) = ws `isPrefix` parts
    in if isP then sepByTitle rest titleParts else []
sepByTitle parts (TitleParam {} : titleParts) = do
    (span, rest) <- splits parts
    restMatches <- sepByTitle rest titleParts
    return $ span:restMatches

-- | Returns the matchables in a list before and after a given matchable.
splitBy :: MatchablePart b -> [MatchablePart a] -> ([MatchablePart a], [MatchablePart a])
splitBy _ [] = ([], [])
splitBy part' (part:parts) =
    if void part == void part'
        then ([], parts)
        else
            let (before, after) = splitBy part' parts
            in (part:before, after)


-- -----------------
-- * Auxiliary matchers

matchAsName :: [MatchablePart a] -> SolverEnv (Maybe Name)
matchAsName [WordP _ w] = return $ Just [w]
matchAsName (WordP _ w : rest) = do
    res <- matchAsName rest
    case res of
        Just ws -> return $ Just (w : ws)
        Nothing -> return Nothing
matchAsName _ = return Nothing

matchAsType :: [MatchablePart a] -> SolverEnv (Maybe Type)
matchAsType parts = do
    res <- matchAsName parts
    case res of
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
matchAsFunctionCall parts funs = concat <$> mapM (matchAsFunctionCall' parts) funs
    where
        matchAsFunctionCall' :: [Annotated MatchablePart] -> FunSignature -> SolverEnv [(FunId, [Annotated Value])]
        matchAsFunctionCall' parts (FunSignature (Title _ funTitle) _) = do
            let possParams = sepByTitle parts funTitle
                fid = getFunId funTitle
            paramMatches <- allNotNull matchAllParams possParams
            let paramCombs = map sequence paramMatches
            return $ concatMap (map (fid,)) paramCombs

        matchAllParams :: [[Annotated MatchablePart]] -> SolverEnv (Maybe [[Annotated Value]])
        matchAllParams partsLists = do
            -- A list of possibilities for each list of matchables.
            resultsLists <- mapM matchAsValue partsLists
            if any null resultsLists
                then return Nothing
                else return $ Just resultsLists


-- -----------------
-- * Value matchers

matchAsPrimitive :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsPrimitive [IntP ann n] = return [IntV ann n]
matchAsPrimitive [FloatP ann n] = return [FloatV ann n]
matchAsPrimitive [CharP ann c] = return [CharV ann c]
matchAsPrimitive [StringP ann@(ln, cn) str] =
    let lvs = zip str [1..]
        locateC = \(char, pos) -> CharV (ln, cn + pos) char
    in return [ListV ann CharT $ map locateC lvs]
matchAsPrimitive [WordP ann w]
    | w == "true" = return [BoolV ann True]
    | w == "false" = return [BoolV ann False]
matchAsPrimitive _ = return []

matchAsVariable :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsVariable parts = do
    res <- matchAsName parts
    let possNames = case res of
            Just name@("the":w:ws) -> if w == "the" then [name] else [name, w:ws]
            Just name -> [name]
            Nothing -> []
        ann = getFirstLocation parts
    names <- filterM variableIsDefined possNames
    return $ map (VarV ann) names

matchAsOperatorCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsOperatorCall parts = do
    let ann = getFirstLocation parts
    operators <- getOperatorSignatures
    r <- matchAsFunctionCall parts operators
    return $ map (uncurry $ OperatorCall ann) r

matchAsIterator :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsIterator (WordP ann "each":parts) = do
    let (before, after) = splitBy (WordP () "in") parts
    befRes <- matchAsType before
    case befRes of
        Just iterType -> do
            possAft <- matchAsValue after
            return $ map (IterV ann iterType) possAft
        Nothing -> return []
matchAsIterator _ = return []

matchAsValue :: [Annotated MatchablePart] -> SolverEnv [Annotated Value]
matchAsValue [ParensP parts] = matchAsValue parts
matchAsValue parts = do
    results <- mapM (\matcher -> matcher parts) [matchAsPrimitive, matchAsVariable, matchAsOperatorCall, matchAsIterator]
    return $ concat results


-- -----------------
-- * Sentence matchers

matchAsProcedureCall :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsProcedureCall parts = do
    let ann = getFirstLocation parts
    procedures <- getProcedureSignatures
    res <- matchAsFunctionCall parts procedures
    toLowerRes <- case parts of
        (WordP fAnn (c:cs) : rest) ->
            if isUpper c
                -- Try matching lower-casing the first letter.
                -- ToDo: this is not neccessary if the first letter of the verb in procedure titles is constrained to lower-case.
                then do
                    let lowerCaseTitle = WordP fAnn (toLower c : cs) : rest
                    matchAsFunctionCall lowerCaseTitle procedures
                else return []
        _ -> return []
    return $ map (uncurry  $ ProcedureCall ann) (res ++ toLowerRes)

matchAsSentence :: [Annotated MatchablePart] -> SolverEnv [Annotated Sentence]
matchAsSentence [ParensP parts] = matchAsSentence parts
matchAsSentence parts = matchAsProcedureCall parts
