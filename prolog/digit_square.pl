?- use_module(library(clpfd)).

digitSquare([A, B, C, D, E, F, G, H, I]):-
    [A, B, C, D, E, F, G, H, I] ins 1..9,
    all_distinct([A, B, C, D, E, F, G, H, I]),
    B + G #= H,
    F #> A,
    TOP_ROW = A+B+C,
    MIDDLE_ROW = D+E+F,
    BOTTOM_ROW = G+H+I,
    TOP_ROW #> MIDDLE_ROW,
    MIDDLE_ROW #> BOTTOM_ROW,
    G in 4\/6\/8..9,
    E in 2..3,
    G #= _*E,
    label([A, B, C, D, E, F, G, H, I]).

    

