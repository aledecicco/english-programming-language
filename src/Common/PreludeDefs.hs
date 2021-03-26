module PreludeDefs where

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

operators :: [(FunctionId, Function)]
operators = map functionFromTuple
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
            [TitleWords ["the", "element", "of"], TitleParam ["l"] (ListT AnyT), TitleWords ["at", "position"], TitleParam ["m"] IntT],
            Operator (\[ListT t, IntT] -> t)
        ),
        (
            [TitleParam ["n"] (ListT AnyT), TitleWords ["appended", "to"], TitleParam ["m"] (ListT AnyT)],
            Operator (\[ListT t, ListT _] -> ListT t)
        )
    ]

procedures :: [(FunctionId, Function)]
procedures = map functionFromTuple
    [
        (
            [TitleWords ["print"], TitleParam ["v"] AnyT],
            Procedure
        )
    ]

isPreludeFunction :: FunctionId -> Bool
isPreludeFunction fid =
    let preludeIds = map fst (operators ++ procedures)
    in fid `elem` preludeIds
