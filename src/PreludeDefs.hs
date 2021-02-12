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
                TitleParam ["list"] (ListT AnyT)
            ]
            (\[ListT t] -> t),
        Operator
            [
                TitleParam ["list", "n"] (ListT AnyT),
                TitleWords ["appended", "to"],
                TitleParam ["list", "m"] (ListT AnyT)
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
                TitleParam ["v"] AnyT
            ],
        Procedure
            [
                TitleWords ["add"],
                TitleParam ["e"] AnyT,
                TitleWords ["to"],
                TitleParam ["l"] (ListT AnyT)
            ],
        Procedure
            [
                TitleWords ["append"],
                TitleParam ["list", "m"] (ListT AnyT),
                TitleWords ["to"],
                TitleParam ["list", "n"] (ListT AnyT)
            ]
    ]
