module BuiltInDefs where

import AST
import Utils (getFunId)

--


-- Auxiliary

functionFromTuple :: ([Bare TitlePart], FunType) -> (FunId, FunSignature)
functionFromTuple (ft, rt) = (getFunId ft, FunSignature (Title () ft) rt)

binaryType :: FunType
binaryType = Operator (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)

relationalType :: FunType
relationalType = Operator (\[_, _] -> BoolT)

--


--

builtInOperators :: [(FunId, FunSignature)]
builtInOperators = map functionFromTuple
    [
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["plus"], TitleParam () ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["times"], TitleParam () ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["minus"], TitleParam () ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["divided", "by"], TitleParam () ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam () ["m"] (AnyT "a"), TitleWords () ["is", "equal", "to"], TitleParam () ["n"] (AnyT "a")],
            relationalType
        ),
        (
            [TitleParam () ["m"] (AnyT "a"), TitleWords () ["is", "not", "equal", "to"], TitleParam () ["n"] (AnyT "a")],
            relationalType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["is", "less", "than"], TitleParam () ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["is", "less", "than", "or", "equal", "to"], TitleParam () ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["is", "greater", "than"], TitleParam () ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam () ["m"] FloatT, TitleWords () ["is", "greater", "than", "or", "equal", "to"], TitleParam () ["n"] FloatT],
            relationalType
        ),
        (
            [TitleWords () ["the", "element", "of"], TitleParam () ["l"] (RefT $ ListT $ AnyT "a"), TitleWords () ["at"], TitleParam () ["m"] IntT],
            Operator (\[RefT (ListT t), IntT] -> RefT t)
        ),
        (
            [TitleWords () ["the", "length", "of"], TitleParam () ["l"] (ListT $ AnyT "a")],
            Operator (\[ListT t] -> IntT)
        ),
        (
            [TitleParam () ["m"] (ListT $ AnyT "a"), TitleWords () ["appended", "to"], TitleParam () ["n"] (ListT $ AnyT "a")],
            Operator (\[ListT t1, ListT t2] -> if t1 == FloatT || t2 == FloatT then ListT FloatT else ListT t1)
        ),
        (
            [TitleWords () ["the", "list", "from"], TitleParam () ["m"] IntT, TitleWords () ["to"], TitleParam () ["n"] IntT],
            Operator (\[IntT, IntT] -> ListT IntT)
        )
    ]

builtInProcedures :: [(FunId, FunSignature)]
builtInProcedures = map functionFromTuple
    [
        (
            [TitleWords () ["print"], TitleParam () ["v"] $ AnyT "a"],
            Procedure
        ),
        (
            [TitleWords () ["swap"], TitleParam () ["v"] $ RefT (AnyT "a"), TitleWords () ["with"], TitleParam () ["w"] $ RefT (AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["add"], TitleParam () ["m"] FloatT, TitleWords () ["to"], TitleParam () ["n"] (RefT FloatT)],
            Procedure
        ),
        (
            [TitleWords () ["multiply"], TitleParam () ["m"] (RefT FloatT), TitleWords () ["by"], TitleParam () ["n"] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["subtract"], TitleParam () ["m"] FloatT, TitleWords () ["from"], TitleParam () ["n"] (RefT FloatT)],
            Procedure
        ),
        (
            [TitleWords () ["divide"], TitleParam () ["m"] (RefT FloatT), TitleWords () ["by"], TitleParam () ["n"] FloatT],
            Procedure
        ),
        (
            [TitleWords () ["append"], TitleParam () ["m"] (ListT $ AnyT "a"), TitleWords () ["to"], TitleParam () ["n"] (RefT . ListT $ AnyT "a")],
            Procedure
        ),
        (
            [TitleWords () ["set"], TitleParam () ["m"] (RefT $ AnyT "a"), TitleWords () ["to"], TitleParam () ["n"] (AnyT "a")],
            Procedure
        )
    ]

isBuiltInFunction :: FunId -> Bool
isBuiltInFunction fid =
    let builtInIds = map fst (builtInOperators ++ builtInProcedures)
    in fid `elem` builtInIds
