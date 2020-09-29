?- use_module(library(clpfd)).
?- use_module(library(clpr)).


%maptree(=(1),t(t(nil,1,nil),1,t(nil,1,nil))).
%maptree(P, nil) :- call(P,nil).
maptree(_,nil).
maptree(P,t(L,X,R)) :- 
    call(P,X),
    maptree(P,L),
    maptree(P,R). 

%the predicate will be false if only one is nill sooo same shape?
maptree(_,nil,nil).
maptree(P,t(L1,X1,R1),t(L2,X2,R2)) :-
    call(P,X1,X2),
    maptree(P,L1,L2),
    maptree(P,R1,R2).


%size( t(t(t(nil, 1, nil), 1, nil), 1, nil) ,X,Y).
size(nil,0,-1).
size(t(L,_,R),N,H):-
    N #> 0,
    H #>= 0,
    H #< N,
    M #= 2^(H+1)-1,
    N #=< M,
    N #= 1+NL+NR,
    HL #>= HR, 
    H #= 1+ HL,
    size(L,NL,HL),
    size(R,NR,HR).

size(t(L,_,R),N,H):-
    N #> 0,
    H #>= 0,
    M #= 2^(H+1)-1,
    N #=< M,
    H #< N,
    N #= 1+NL+NR,
    HL #< HR, 
    H #= 1+ HR,
    size(L,NL,HL),
    size(R,NR,HR).

size2(X,N,H) :-
    H #=< N + 1,
    M = 2^(H+1)-1,
    N #=< M,
    nodes_nbr(X,N),
    height(X,H).

height(nil,(-1)).
height(t(L,_,R),H):-
    HL #>= HR, 
    H #= 1+ HL, 
    height(L,HL),
    height(R,HR).

height(t(L,_,R),H):-
    H #> 0,
    HL #< HR, 
    H #= 1+ HR,
    height(L,HL),
    height(R,HR).

nodes_nbr(nil,0).
nodes_nbr(t(L,_,R),N):-
    N #> 0,
    N #= 1+NL+NR,
    nodes_nbr(L,NL),
    nodes_nbr(R,NR).




% ?- size(T, 3, 2), maptree(=(1), T).
% T = t(nil, 1, t(nil, 1, t(nil, 1, nil))) ;
% T = t(nil, 1, t(t(nil, 1, nil), 1, nil)) ;
% T = t(t(nil, 1, t(nil, 1, nil)), 1, nil) ;
% T = t(t(t(nil, 1, nil), 1, nil), 1, nil) ;

