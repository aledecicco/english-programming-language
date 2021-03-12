module Utils where

import Data.List ( intercalate )
import Data.Char ( toLower )

import AST
import ParserEnv ( ParserEnv )

-- Returns whether two words match
isWord :: String -> String -> Bool
isWord w1 w2 = map toLower w1 == map toLower w2

getTitle :: Function -> Title
getTitle (Operator t _) = t
getTitle (Procedure t) = t

getFunctionId :: Title -> FunctionId
getFunctionId t = intercalate "_" (getFunctionIdParts t)
    where
        getFunctionIdParts :: Title -> [FunctionId]
        getFunctionIdParts (TitleWords w : ts) = w ++ getFunctionIdParts ts
        getFunctionIdParts (TitleParam {} : ts) = "%" : getFunctionIdParts ts
