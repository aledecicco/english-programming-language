module PreludeDefs where

import Types

--


-- Definitions

-- List of predefined operators (functions that return a value)
operators :: [(String, Function)]
operators =
    [
        (
            "%_plus_%",
            Operator
                [
                    TitleParam ["m"] FloatT,
                    TitleWords ["plus"],
                    TitleParam ["n"] FloatT
                ]
                (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)
        ),
        (
            "the_first_element_of_%",
            Operator
                [
                    TitleWords ["the", "first", "element", "of"],
                    TitleParam ["l"] (ListT AnyT)
                ]
                (\[ListT t] -> t)
        ),
        (
            "%_appended_to_%",
            Operator
                [
                    TitleParam ["n"] (ListT AnyT),
                    TitleWords ["appended", "to"],
                    TitleParam ["m"] (ListT AnyT)
                ]
                (\[ListT t, ListT _] -> t)
        )
    ]

-- List of predefined procedures (functions that have no return value)
procedures :: [(String, Function)]
procedures =
    [
        (
            "print_%",
            Procedure
                [
                    TitleWords ["print"],
                    TitleParam ["v"] AnyT
                ]
        ),
        (
            "add_%_to_%",
            Procedure
                [
                    TitleWords ["add"],
                    TitleParam ["e"] AnyT,
                    TitleWords ["to"],
                    TitleParam ["l"] (ListT AnyT)
                ]
        ),
        (
            "append_%_to_%",
            Procedure
                [
                    TitleWords ["append"],
                    TitleParam ["list", "m"] (ListT AnyT),
                    TitleWords ["to"],
                    TitleParam ["list", "n"] (ListT AnyT)
                ]
        )
    ]
