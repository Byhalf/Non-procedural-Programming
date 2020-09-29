?- use_module(library(clpfd)).
?- use_module(library(clpr)).


%no_divide(N,K) :- N mod K #\=0.
%no_divide(L,N) :- maplist(no_divide(N),L).

%sqrt2(N,K):- K#>=0, K#=<N, K1 #= K+1, K1*K1>N.
perm([],[]).
perm(L,[X|N]) :- same_length(L,[X|N]), select(X, L, M), perm(M,N).

contains(L,X) :- member(X,L).
subset(L,M) :- maplist(contains(M),L).

%perm(L,M):- same_length(L, M), subset(L,M),subset(M,L).

add(X,Y,Z):- {X+Y=Z}.
vsum(V,W,X):- maplist(add,V,W,X).

%same for mult
mult(X,Y,Z):- {X*Y=Z}.
vmult(V,W,X):- maplist(mult,V,W,X).


%dot operation [2,4].[5,10]
dot(V,W,X):- maplist(mul,V,W,L),foldl(add,L,0,X).

accum(X,Y,A,B):- {B=A+X*Y}.
%dot(V,W,X):- foldl(accum,V,W,0,X).

has_len(N,L):-length(L,N).
hasdim(M,N):- length(M, N), maplist(has_len(N),M).

is_zero_vec(V,maplist(=(0),V)).
is_zero(M):- maplist(is_zero_vec,M).
