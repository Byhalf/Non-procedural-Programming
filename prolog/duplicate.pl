dup([],[]).
dup([A|L],[A,A|M]):- dup(L,M).

dedup([],[]).
dedup([A],[A]).
dedup([A,A|L],[A|M]):- dedup([A|L],[A|M]).
dedup([A,B|L],[A,B|M]):- dif(A,B),dedup([B|L],[B|M]).



group_first([],[]).
group_first([A,A|L],[A,A|M]) :- group_first([A|L],[A|M]).
group_first([A,B|_],[A]) :- dif(A,B).
group_first([A],[A]).


group([],[]).
group(L,[X|M]) :- dif(X,[]),group_first(L,X), append(X,Y,L),group(Y,M).


