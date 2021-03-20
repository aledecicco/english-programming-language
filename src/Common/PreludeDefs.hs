module PreludeDefs where

import AST

--


-- Definitions

-- List of predefined operators (functions that return a value)
operators :: [(String, Function)]
operators =
    [
        (
            "%_plus_%",
            Function
                [
                    TitleParam ["m"] FloatT,
                    TitleWords ["plus"],
                    TitleParam ["n"] FloatT
                ] $
                Operator (\[tM, tN] -> if tM == FloatT || tN == FloatT then FloatT else IntT)
        ),
        (
            "the_first_element_of_%",
            Function
                [
                    TitleWords ["the", "first", "element", "of"],
                    TitleParam ["l"] (ListT AnyT)
                ] $
                Operator (\[ListT t] -> t)
        ),
        (
            "%_appended_to_%",
            Function
                [
                    TitleParam ["n"] (ListT AnyT),
                    TitleWords ["appended", "to"],
                    TitleParam ["m"] (ListT AnyT)
                ] $
                Operator (\[ListT t, ListT _] -> t)
        )
    ]

-- List of predefined procedures (functions that have no return value)
procedures :: [(String, Function)]
procedures =
    [
        (
            "print_%",
            Function
                [
                    TitleWords ["print"],
                    TitleParam ["v"] AnyT
                ]
                Procedure
        ),
        (
            "add_%_to_%",
            Function
                [
                    TitleWords ["add"],
                    TitleParam ["e"] AnyT,
                    TitleWords ["to"],
                    TitleParam ["l"] (ListT AnyT)
                ]
                Procedure
        ),
        (
            "append_%_to_%",
            Function
                [
                    TitleWords ["append"],
                    TitleParam ["list", "m"] (ListT AnyT),
                    TitleWords ["to"],
                    TitleParam ["list", "n"] (ListT AnyT)
                ]
                Procedure
        )
    ]
