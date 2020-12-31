module PreludeDefs where

import Types

--


-- Definitions

-- List of predefined operators (functions that return a value)
operators :: [Function]
operators =
    [
        Operator
            [
                TitleParam ["number", "m"] IntT,
                TitleWords ["plus"],
                TitleParam ["number", "n"] IntT
            ]
            (const IntT),
        Operator
            [
                TitleWords ["the", "first", "element", "of"],
                TitleParam ["list"] (ListT (AnyT "t1"))
            ]
            (\[ListT t] -> t),
        Operator
            [
                TitleParam ["list", "n"] (ListT (AnyT "t1")),
                TitleWords ["appended", "to"],
                TitleParam ["list", "m"] (ListT (AnyT "t1"))
            ]
            (\[ListT t, ListT _] -> t)
    ]

-- List of predefined procedures (functions that have no return value)
procedures :: [Function]
procedures =
    [
        Procedure
            [
                TitleWords ["print"],
                TitleParam ["v"] (AnyT "t1")
            ],
        Procedure
            [
                TitleWords ["add"],
                TitleParam ["e"] (AnyT "t1"),
                TitleWords ["to"],
                TitleParam ["l"] (ListT (AnyT "t1"))
            ],
        Procedure
            [
                TitleWords ["append"],
                TitleParam ["list", "n"] (ListT (AnyT "t1")),
                TitleWords ["to"],
                TitleParam ["list", "m"] (ListT (AnyT "t1"))
            ]
    ]
