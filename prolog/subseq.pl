sublist(L,M):- append(M,_,L), M=[_|_].
sublist([_|L],M):-sublist(L,M).

%all_same([],[]).
%all_same([X|L],[X|M]) :- all_same(L,M).

%cut([],_,[]).
%cut([S|L],S,M):- all_same(L,M).
%cut([X|L],S,M):- dif(X,S),cut(L,S,M).


%subseq(_,[]).
%subseq(L,[A|M]) :- select(A,L,M), cut(L,A,Y),subseq(Y,M).

subseq(_,[]).
subseq([A|L],[A|M]):- subseq(L,M).
subseq([A|L],[B|M]):- dif(A,B),subseq(L,[B|M]).

disjoint([],[],[]).
disjoint([A|L],[A|M],N):-disjoint(L,M,N).
disjoint([A|L],M,[A|N]):-disjoint(L,M,N).

 

