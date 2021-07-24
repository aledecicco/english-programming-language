module Utils where

import Data.List ( intercalate )
import Data.Maybe ( catMaybes )

import AST

--


--

-- Gets a function's id from its title
getFunId :: [TitlePart a] -> FunId
getFunId ts = intercalate "_" (getFunIdParts ts)
    where
        getFunIdParts :: [TitlePart a] -> [FunId]
        getFunIdParts [] = []
        getFunIdParts (TitleWords _ w : ts) = w ++ getFunIdParts ts
        getFunIdParts (TitleParam {} : ts) = "%" : getFunIdParts ts

-- Receives a computation and tries it on each element of a list, returning the first non-empty result
firstNotNull :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstNotNull f [] = return Nothing
firstNotNull f (x:xs) = do
    r <- f x
    case r of
        Just x' -> return $ Just x'
        Nothing -> firstNotNull f xs

-- Receives a computation and tries it on each element of a list, returning all the results
-- but only if all of them yielded a result
allOrNone :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allOrNone f xs = sequence <$> traverse f xs

-- Receives a computation and tries it on each element of a list, returning all the non-empty results
allNotNull :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
allNotNull f es = catMaybes <$> mapM f es

--
