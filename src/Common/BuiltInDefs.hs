{-|
Module      : BuiltInDefs
Copyright   : (c) Alejandro De Cicco, 2021
License     : MIT
Maintainer  : alejandrodecicco99@gmail.com

Signatures of functions defined in the language's prelude.
-}

module BuiltInDefs where

import AST
    ( FunType(..),
      FunSignature(..),
      Title(Title),
      TitlePart(..),
      Type(AnyT, BoolT, IntT, FloatT, ListT, RefT),
      Bare,
      FunId )
import Utils (getFunId)


-- -----------------
-- * Auxiliary

-- | Takes a title and a function type, and returns the function's id and its signature.
functionFromTuple :: ([Bare TitlePart], FunType) -> (FunId, FunSignature)
functionFromTuple (title, returnType) = (getFunId title, FunSignature (Title () title) returnType)

-- | The type of a function that takes two numeric arguments and returns a float if either of them is a float, and an int otherwise.
binaryType :: FunType
binaryType = Operator (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)

-- | The type of a function that compares its two arguments and returns a bool.
relationalType :: FunType
relationalType = Operator (\[_, _] -> BoolT)


-- -----------------
-- * Definitions

builtInOperators :: [(FunId, FunSignature)]
builtInOperators =
    map functionFromTuple
    [
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
            Operator (\[_, _] -> FloatT)
        ),
        (
            [TitleWords () ["the", "quotient", "of"], TitleParam () [] IntT, TitleWords () ["and"], TitleParam () [] IntT],
            Operator (\[_, _] -> IntT)
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
            [TitleWords () ["the", "element", "of"], TitleParam () [] (RefT . ListT $ AnyT "a"), TitleWords () ["at"], TitleParam () [] IntT],
            Operator (\[RefT (ListT t), IntT] -> RefT t)
        ),
        (
            [TitleWords () ["the", "length", "of"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[ListT t] -> IntT)
        ),
        (
            [TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["appended", "to"], TitleParam () [] (ListT $ AnyT "a")],
            Operator (\[ListT t1, ListT t2] -> if t1 == FloatT || t2 == FloatT then ListT FloatT else ListT t1)
        ),
        (
            [TitleWords () ["the", "list", "from"], TitleParam () [] IntT, TitleWords () ["to"], TitleParam () [] IntT],
            Operator (\[IntT, IntT] -> ListT IntT)
        )
    ]

builtInProcedures :: [(FunId, FunSignature)]
builtInProcedures = map functionFromTuple
    [
        (
            [TitleWords () ["print"], TitleParam () [] $ AnyT "a"],
            Procedure
        ),
        (
            [TitleWords () ["swap"], TitleParam () [] $ RefT (AnyT "a"), TitleWords () ["with"], TitleParam () [] $ RefT (AnyT "a")],
            Procedure
        ),
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
            [TitleWords () ["append"], TitleParam () [] (ListT $ AnyT "a"), TitleWords () ["to"], TitleParam () [] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["set"], TitleParam () [] (RefT $ AnyT "a"), TitleWords () ["to"], TitleParam () [] (AnyT "a")],
            Procedure
        )
    ]

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
