?- use_module(library(clpfd)).
?- use_module(library(clpr)).


%is_vector meaning dimY == 0
is_vector(Y):- maplist(number,Y).


% dim_y([],0). %how to make it say 0 for non nested list
% dim_y(X,0):- is_vector(X). 
% dim_y([X|Y],E):-is_vector(X), length([X|Y],L),L #= 1, length(X,E), dim_y(Y,0).
% dim_y([X|Y],E):-is_vector(X), length([X|Y],L),L #> 1 , length(X,E), dim_y(Y,E).

reverse_length(N,L):-length(L,N).

dim_Y(X,0):- is_vector(X).
dim_Y([M|N],E):- 
    length(M,E),
    maplist(is_vector,[M|N]),
    maplist(reverse_length(E),[M|N]).

dim_matrix([],0,0).
dim_matrix(M,X,Y) :- 
    length(M, X),
    dim_Y(M,Y).
    