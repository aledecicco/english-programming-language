module Utils where

import Data.Char ( toLower )

import Types ( Line(..), LineNumber )

-- Returns whether two words match
isWord :: String -> String -> Bool
isWord w1 w2 = map toLower w1 == map toLower w2

getLineNumber :: Line a -> LineNumber
getLineNumber (Line ln _) = ln

getLineContent :: Line a -> a
getLineContent (Line _ c) = c
