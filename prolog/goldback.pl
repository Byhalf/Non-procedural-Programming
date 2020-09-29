?- use_module(library(clpfd)).

divisibles(X,[N]) :-
    X1 #= X mod N,
    X1 #\=0.
divisibles(X,[N|L]) :- 
    X1 #= X mod N,
    X1 #\=0,
    divisibles(X,L).

make_list(N,N,[N]).
make_list(N1,N2,[N1|L]):-
    N2 #> N1,
    N3 #= N1+1,
    make_list(N3,N2,L).
    
prime(2).
prime(X) :-
    X1 #= X //2 + 1,
    make_list(2,X1,P),
    divisibles(X,P).

    
    
goldbach(P,Q,E):-
    E in 4..sup,
    E mod 2 #= 0,
    P + Q #= E,
    P #=< Q,
    Q #>= 2,
    P #>= 2,
    P #=< E,
    Q #=< E,
    prime(Q),
    prime(P).

