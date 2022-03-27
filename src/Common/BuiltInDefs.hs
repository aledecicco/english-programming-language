{-# LANGUAGE LiberalTypeSynonyms  #-}
{-|
Module      : BuiltInDefs
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Signatures of functions defined in the language's prelude.
-}

module BuiltInDefs where

import AST
import Utils (getFunId)


-- -----------------
-- * Auxiliary

-- | Takes a title and a function type, and returns the function's id and its signature.
functionFromTuple :: (Bare Title, FunType) -> (FunId, FunSignature)
functionFromTuple (title, returnType) = (getFunId title, FunSignature title returnType)

-- | The type of a function that takes two numeric arguments and returns a float if either of them is a float, and an int otherwise.
binaryType :: FunType
binaryType = Operator (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)

-- | The type of a function that compares its two arguments and returns a bool.
relationalType :: FunType
relationalType = Operator (\[_, _] -> BoolT)


-- -----------------
-- * Specific definitions

charOperators :: [(Bare Title, FunType)]
charOperators =
    [
        (
            [TitleParam () [] CharT, TitleWords () ["in", "uppercase"]],
            Operator (const CharT)
        ),
        (
            [TitleParam () [] CharT, TitleWords () ["in", "lowercase"]],
            Operator (const CharT)
        )
    ]

charProcedures :: [(Bare Title, FunType)]
charProcedures =
    [
        (
            [TitleWords () ["transform"], TitleParam () [] (RefT CharT), TitleWords () ["to", "uppercase"]],
            Procedure
        ),
        (
            [TitleWords () ["transform"], TitleParam () [] (RefT CharT), TitleWords () ["to", "lowercase"]],
            Procedure
        )
    ]

boolOperators :: [(Bare Title, FunType)]
boolOperators =
    [
        (
            [TitleParam () [] BoolT, TitleWords () ["is", "true"]],
            Operator (const BoolT)
        ),
        (
            [TitleParam () [] BoolT, TitleWords () ["is", "false"]],
            Operator (const BoolT)
        ),
        (
            [TitleWords () ["whether"], TitleParam () [] BoolT],
            Operator (const BoolT)
        ),
        (
            [TitleWords () ["not"], TitleParam () [] BoolT],
            Operator (const BoolT)
        ),
        (
            [TitleParam () [] BoolT, TitleWords () ["negated"]],
            Operator (const BoolT)
        ),
        (
            [TitleParam () [] BoolT, TitleWords () ["and"], TitleParam () [] BoolT],
            relationalType
        ),
        (
            [TitleParam () [] BoolT, TitleWords () ["or"], TitleParam () [] BoolT],
            relationalType
        ),
        (
            [TitleParam () [] BoolT, TitleWords () ["or"], TitleParam () [] BoolT, TitleWords () ["but", "not", "both"]],
            relationalType
        )
    ]

boolProcedures :: [(Bare Title, FunType)]
boolProcedures =
    [
        (
            [TitleWords () ["negate"], TitleParam () [] (RefT BoolT)],
            Procedure
        )
    ]

listOperators :: [(Bare Title, FunType)]
listOperators =
    [
        (
            [TitleWords () ["the", "length", "of"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (const IntT)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["is", "empty"]],
            Operator (const BoolT)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["is", "not", "empty"]],
            Operator (const BoolT)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["contains"], TitleParam () [] (AnyT "a")],
            Operator (const BoolT)
        ),
        (
            [TitleWords () ["the", "element", "of"], TitleParam () [] (RefT . ListT $ AnyT "a"), TitleWords () ["at"], TitleParam () [] IntT],
            Operator (\[RefT (ListT t), _] -> RefT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "the", "element", "at"], TitleParam () [] IntT],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "the", "first", "apparition", "of"], TitleParam () [] (AnyT "a")],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "all", "apparitions", "of"], TitleParam () [] (AnyT "a")],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "its", "first", "element"]],
            Operator (\[ListT t] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "its", "last", "element"]],
            Operator (\[ListT t] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["with"], TitleParam () [] (AnyT "a"), TitleWords () ["added", "at", "the", "beggining"]],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["with"], TitleParam () [] (AnyT "a"), TitleWords () ["added", "at", "the", "end"]],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["with"], TitleParam () [] (AnyT "a"), TitleWords () ["added", "at"], TitleParam () [] IntT],
            Operator (\[ListT t, _, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["appended", "to"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[ListT t1, ListT t2] -> if t1 == FloatT || t2 == FloatT then ListT FloatT else ListT t1)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["prepended", "to"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[ListT t1, ListT t2] -> if t1 == FloatT || t2 == FloatT then ListT FloatT else ListT t1)
        ),
        (
            [TitleWords () ["the", "first"],  TitleParam () [] IntT, TitleWords () ["elements", "of"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[_, ListT t] -> ListT t)
        ),
        (
            [TitleWords () ["the", "last"],  TitleParam () [] IntT, TitleWords () ["elements", "of"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[_, ListT t] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "its", "first"],  TitleParam () [] IntT, TitleWords () ["elements"]],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["without", "its", "last"],  TitleParam () [] IntT, TitleWords () ["elements"]],
            Operator (\[ListT t, _] -> ListT t)
        ),
        (
            [TitleWords () ["the", "list", "from"], TitleParam () [] IntT, TitleWords () ["to"], TitleParam () [] IntT],
            Operator (const $ ListT IntT)
        )
    ]

listProcedures :: [(Bare Title, FunType)]
listProcedures =
    [
        (
            [TitleWords () ["empty", "out"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "element", "at"], TitleParam () [] IntT, TitleWords () ["from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "first", "apparition", "of"], TitleParam () [] (AnyT "a"), TitleWords () ["from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "all", "apparitions", "of"], TitleParam () [] (AnyT "a"), TitleWords () ["from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "first", "element", "from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "last", "element", "from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["add"],  TitleParam () [] (AnyT "a"), TitleWords () ["at", "the", "beginning", "of"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["add"],  TitleParam () [] (AnyT "a"), TitleWords () ["at", "the", "end", "of"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["add"],  TitleParam () [] (AnyT "a"), TitleWords () ["to"], TitleParam () [] (RefT . ListT $ AnyT "a"), TitleWords () ["at"], TitleParam () [] IntT],
            Procedure
        ),
        (
            [TitleWords () ["append"], TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["to"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["prepend"], TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["to"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["leave", "only", "the", "first"],  TitleParam () [] IntT, TitleWords () ["elements", "in"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["leave", "only", "the", "last"],  TitleParam () [] IntT, TitleWords () ["elements", "in"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "first"], TitleParam () [] IntT, TitleWords () ["elements", "from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["remove", "the", "last"], TitleParam () [] IntT, TitleWords () ["elements", "from"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        )
    ]

numberOperators :: [(Bare Title, FunType)]
numberOperators =
    [
        (
            [TitleParam () [] FloatT, TitleWords () ["raised", "to", "the", "power", "of"], TitleParam () [] FloatT],
            binaryType
        ),
        (
            [TitleWords () ["the", "quotient", "of"], TitleParam () [] IntT, TitleWords () ["and"], TitleParam () [] IntT],
            Operator (const IntT)
        ),
        (
            [TitleParam () [] IntT, TitleWords () ["module"], TitleParam () [] IntT],
            Operator (const IntT)
        ),
        (
            [TitleWords () ["the", "remainder", "of", "dividing"], TitleParam () [] IntT, TitleWords () ["by"], TitleParam () [] IntT],
            Operator (const IntT)
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["plus"], TitleParam () [] FloatT],
            binaryType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["times"], TitleParam () [] FloatT],
            binaryType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["minus"], TitleParam () [] FloatT],
            binaryType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["divided", "by"], TitleParam () [] FloatT],
            Operator (const FloatT)
        ),
        (
            [TitleParam () [] (AnyT "a"), TitleWords () ["is", "equal", "to"], TitleParam () [] (AnyT "a")],
            relationalType
        ),
        (
            [TitleParam () [] (AnyT "a"), TitleWords () ["is", "not", "equal", "to"], TitleParam () [] (AnyT "a")],
            relationalType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["is", "less", "than"], TitleParam () [] FloatT],
            relationalType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["is", "less", "than", "or", "equal", "to"], TitleParam () [] FloatT],
            relationalType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["is", "greater", "than"], TitleParam () [] FloatT],
            relationalType
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["is", "greater", "than", "or", "equal", "to"], TitleParam () [] FloatT],
            relationalType
        ),
        (
            [TitleWords () ["the", "square", "root", "of"], TitleParam () [] FloatT],
            Operator (const FloatT)
        ),
        (
            [TitleWords () ["the", "ceiling", "of"], TitleParam () [] FloatT],
            Operator (const IntT)
        ),
        (
            [TitleWords () ["the", "floor", "of"], TitleParam () [] FloatT],
            Operator (const IntT)
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["rounded"]],
            Operator (const IntT)
        ),
        (
            [TitleParam () [] FloatT, TitleWords () ["truncated"]],
            Operator (const IntT)
        ),
        (
            [TitleWords () ["the", "absolute", "value", "of"], TitleParam () [] FloatT],
            Operator (\[t] -> t)
        )
    ]

numberProcedures :: [(Bare Title, FunType)]
numberProcedures =
    [

        (
            [TitleWords () ["add"], TitleParam () [] FloatT, TitleWords () ["to"], TitleParam () [] (RefT FloatT)],
            Procedure
        ),
        (
            [TitleWords () ["multiply"], TitleParam () [] (RefT FloatT), TitleWords () ["by"], TitleParam () [] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["subtract"], TitleParam () [] FloatT, TitleWords () ["from"], TitleParam () [] (RefT FloatT)],
            Procedure
        ),
        (
            [TitleWords () ["divide"], TitleParam () [] (RefT FloatT), TitleWords () ["by"], TitleParam () [] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["quotient"], TitleParam () [] (RefT FloatT), TitleWords () ["by"], TitleParam () [] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["raise"], TitleParam () [] (RefT FloatT), TitleWords () ["to", "the", "power", "of"], TitleParam () [] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["round"], TitleParam () [] (RefT FloatT)],
            Operator (\[_] -> IntT)
        ),
        (
            [TitleWords () ["round"], TitleParam () [] (RefT FloatT), TitleWords () ["up"]],
            Operator (\[_] -> IntT)
        ),
        (
            [TitleWords () ["round"], TitleParam () [] (RefT FloatT), TitleWords () ["down"]],
            Operator (\[_] -> IntT)
        ),
        (
            [TitleWords () ["truncate"], TitleParam () [] (RefT FloatT)],
            Operator (\[_] -> IntT)
        )
    ]

generalOperators :: [(Bare Title, FunType)]
generalOperators =
    [
        (
            [TitleParam () [] (AnyT "a"), TitleWords () ["as", "a", "string"]],
            Procedure
        )
    ]

generalProcedures :: [(Bare Title, FunType)]
generalProcedures =
    [
        (
            [TitleWords () ["print"], TitleParam () [] (AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["swap"], TitleParam () [] (RefT $ AnyT "a"), TitleWords () ["with"], TitleParam () [] (RefT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["set"], TitleParam () [] (RefT $ AnyT "a"), TitleWords () ["to"], TitleParam () [] (AnyT "a")],
            Procedure
        )
    ]


-- -----------------
-- * Functions

builtInOperators :: [(FunId, FunSignature)]
builtInOperators = map functionFromTuple $ charOperators ++ boolOperators ++ listOperators ++ numberOperators ++ generalOperators

builtInProcedures :: [(FunId, FunSignature)]
builtInProcedures = map functionFromTuple $ charProcedures ++ boolProcedures ++ listProcedures ++ numberProcedures ++ generalProcedures

builtInFunctions :: [(FunId, FunSignature)]
builtInFunctions = builtInOperators ++ builtInProcedures


-- -----------------
-- * Checks on functions

isBuiltInOperator :: FunId -> Bool
isBuiltInOperator fid =
    let builtInIds = map fst builtInOperators
    in fid `elem` builtInIds

isBuiltInProcedure :: FunId -> Bool
isBuiltInProcedure fid =
    let builtInIds = map fst builtInProcedures
    in fid `elem` builtInIds
