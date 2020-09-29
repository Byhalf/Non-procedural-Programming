second_last(X,[X,_]).
second_last(X,[_|L]) :- second_last(X,L).

%Write a predicate next_to(X, Y, L) that is true if elements
% X and Y appear in L, with Y immediately after X.

next_to(X,Y,[X,Y]).
next_to(X,Y,[X,Y|_]).
next_to(X,Y,[_|L]) :- next_to(X,Y,L).

%Write a predicate even_length(L) that is true if the
% length of L is an even integer.
even_length([]).
even_length([_,_|L]):-even_length(L).

%Write a predicate same_length(L, M) that is true 
%if L and M have the same length.
same_length([],[]).
same_length([_|M],[_|L]):-same_length(M,L).

%Write a predicate longer(L, M) that is true if L is longer than M.
% Do not use any numbers in your solution.
longer([_],[]).
longer([_|_],[_]).
longer([_|L],[_,M]) :- longer(L,M).
%***************************why is L [_,[]]**********

%Write a predicate double_length(L, M) that is true if L is 
%twice as long as M.Do not use any numbers in your solution.
double_length([],[]).
double_length([_,_|L],[_|M]) :- double_length(L,M).

reverse([],[]).
reverse([X|L],M) :- reverse(L,LR), append(LR,[X],M).

%whyyyyyyy does it not work
rotate([],[]).
rotate(L,[E|M]) :- reverse(L,[X|_]),append([X],M,[E|M]).

select(X,L,M) :- same_length(L,[_|M]),append(A,B,M),append(A,[X|B],L).

select(X,L,Y,M) :- same_length(L,M), select(X,L,A),select(Y,M,A).

all_same([]).
all_same([_]).
all_same([X,X|L]) :- all_same([X|L]).


all_different(_,[]).
all_different(X,[Y|L]):- dif(X,Y),all_different(X,L).


all_different([]).
all_different([X|L]) :- all_different(X, L), all_different(L).



zip([],[]).
zip(pair(X, Y),).
