module Utils where

import Control.Monad ( msum )
import Data.List ( intercalate )

import AST
import Env

--


-- Functions

getFunctionId :: Title -> FunctionId
getFunctionId t = intercalate "_" (getFunctionIdParts t)
    where
        getFunctionIdParts :: Title -> [FunctionId]
        getFunctionIdParts (TitleWords w : ts) = w ++ getFunctionIdParts ts
        getFunctionIdParts (TitleParam {} : ts) = "%" : getFunctionIdParts ts

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
