module Utils where

import Control.Monad ( msum, void )
import Data.List ( intercalate )

import AST

--


-- Functions

getFunId :: [TitlePart a] -> FunId
getFunId ts = intercalate "_" (getFunIdParts ts)
    where
        getFunIdParts :: [TitlePart a] -> [FunId]
        getFunIdParts [] = []
        getFunIdParts (TitleWords _ w : ts) = w ++ getFunIdParts ts
        getFunIdParts (TitleParam {} : ts) = "%" : getFunIdParts ts

--


-- Actions

-- Receives a computation and tries it on each element of a list, returning the first non-empty result
firstNotNull :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstNotNull f xs = msum <$> traverse f xs

-- Receives a computation and tries it on each element of a list, returning all the results
-- but only if all of them yielded a result
allOrNone :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe [b])
allOrNone f xs = sequence <$> traverse f xs

--
