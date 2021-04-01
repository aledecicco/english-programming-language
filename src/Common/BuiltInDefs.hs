module BuiltInDefs where

import AST
import Utils (getFunctionId)

--


-- Auxiliary

functionFromTuple :: (Title, ReturnType) -> (FunctionId, Function)
functionFromTuple (ft, rt) = (getFunctionId ft, Function ft rt)

binaryType :: ReturnType
binaryType = Operator (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)

relationalType :: ReturnType
relationalType = Operator (\[_, _] -> BoolT)

--


--

builtInOperators :: [(FunctionId, Function)]
builtInOperators = map functionFromTuple
    [
        (
            [TitleParam ["m"] FloatT, TitleWords ["plus"], TitleParam ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["times"], TitleParam ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["minus"], TitleParam ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["divided", "by"], TitleParam ["n"] FloatT],
            binaryType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["is", "less", "than"], TitleParam ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["is", "less", "than", "or", "equal", "to"], TitleParam ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["is", "greater", "than"], TitleParam ["n"] FloatT],
            relationalType
        ),
        (
            [TitleParam ["m"] FloatT, TitleWords ["is", "greater", "than", "or", "equal", "to"], TitleParam ["n"] FloatT],
            relationalType
        ),
        (
            [TitleWords ["The", "element", "of"], TitleParam ["l"] (ListT AnyT), TitleWords ["at", "position"], TitleParam ["m"] IntT],
            Operator (\[ListT t, IntT] -> t)
        ),
        (
            [TitleParam ["n"] (ListT AnyT), TitleWords ["appended", "to"], TitleParam ["m"] (ListT AnyT)],
            Operator (\[ListT t, ListT _] -> ListT t)
        )
    ]

builtInProcedures :: [(FunctionId, Function)]
builtInProcedures = map functionFromTuple
    [
        (
            [TitleWords ["print"], TitleParam ["v"] AnyT],
            Procedure
        )
    ]

isBuiltInFunction :: FunctionId -> Bool
isBuiltInFunction fid =
    let builtInIds = map fst (builtInOperators ++ builtInProcedures)
    in fid `elem` builtInIds
