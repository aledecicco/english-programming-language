module Utils where

import Control.Monad ( msum )
import Data.List ( intercalate )

import AST
import Env

--


-- Functions

getFunId :: Title -> FunId
getFunId t = intercalate "_" (getFunIdParts t)
    where
        getFunIdParts :: Title -> [FunId]
        getFunIdParts [] = []
        getFunIdParts (TitleWords w : ts) = w ++ getFunIdParts ts
        getFunIdParts (TitleParam {} : ts) = "%" : getFunIdParts ts

--


-- Actions

-- Receives a computation and tries it on each element of a list, returning the first non-empty result
firstNotNull :: Monad m => (a -> Env x y m (Maybe b)) -> [a] -> Env x y m (Maybe b)
firstNotNull f xs = msum <$> traverse f xs

-- Receives a computation and tries it on each element of a list, returning all the results
-- but only if all of them yielded a result
allOrNone :: Monad m => (a -> Env x y m (Maybe b)) -> [a] -> Env x y m (Maybe [b])
allOrNone f xs = sequence <$> traverse f xs

--
