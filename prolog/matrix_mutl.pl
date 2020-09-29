?- use_module(library(clpfd)).
?- use_module(library(clpr)).


% helper predicates
add(I, J, K) :- { K = I + J }.
mul(I, J, K) :- { K = I * J }.


% dot(V, W, X): X is the dot product of vectors V and W
dot(V, W, X) :- maplist(mul, V, W, Z), foldl(add, Z, 0, X).



%is_vector meaning dimY == 0
%is_vector(Y):- maplist(number,Y).


% dim_Y(X,0):-length(X,Y), Y#>0, is_vector(X). %>0 so empty list has only one solution
% dim_Y([M|N],E):-
%     length(M,E),
%     maplist(is_vector,[M|N]),
%     maplist(reverse_length(E),[M|N]).

% dim_matrix([],0,0).
% dim_matrix(M,X,Y) :- 
%     length(M, X),
%     dim_Y(M,Y).
    

% keep_first([E|_],E).     
% keep_first_y(M,N) :-
%     maplist(keep_first,M,N).

% drop_first([_|M],M).
% drop_first_y(M,N):-
%     maplist(drop_first,M,N).

multmv([M|O],V,N):-
    length(M,X1),
    length(V,X1),
    maplist(dot(V),[M|O],N).



dim_X(M,X):- length(M,X).
dim_Y([],0).
dim_Y([M|_],Y):- length(M,Y).

% helper predicates
elem(X, [X]).
prepend(X, L, [X | L]).

% base case: transpose 1 x N -> N x 1
trans([R], L) :- maplist(elem, R, L).

% recursive case
trans([R | M], N) :-
    maplist(prepend,R, N1, N),
    trans(M, N1).

reverse_length(N,L):-length(L,N).

mul_mm(M,N,R):-
    M = [MR | _], N = [NR | _], R = [RR | _],
    same_length(M, R), same_length(MR,N), same_length(RR,NR),
    trans(N,NT),
    maplist(multmv(NT),M,R).


%[[1,2,3],[4,5,6],[7,8,9]] [2,1,3]