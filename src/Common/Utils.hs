module Utils where

import Data.List ( intercalate )

import AST

--


--

getFunctionId :: Title -> FunctionId
getFunctionId t = intercalate "_" (getFunctionIdParts t)
    where
        getFunctionIdParts :: Title -> [FunctionId]
        getFunctionIdParts (TitleWords w : ts) = w ++ getFunctionIdParts ts
        getFunctionIdParts (TitleParam {} : ts) = "%" : getFunctionIdParts ts
