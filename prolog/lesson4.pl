?- use_module(library(clpfd)).
?- use_module(library(clpr)).

%
%
%
sum([],0).
sum([X|L],S) :- sum(L,S1), S #= X+S1. %we will use this for now for [3,4,5] %3+4+5+0 %right associative
sum([X|L],S) :- S #= X+S1,sum(L,S1) . %better

%same results because addition is associative

sum2(L,S) :- sum2(L,0,S).
sum2([],S,S).
sum2([X|L],A,S) :- A1 #= X+A, sum2(L,A1,S). % A an accumulator 0+3+4+5 %left associative

%reverse([],[]).
%reverse([X|_],M) :- reverse(M,M1),append(M1,[X],M). %quadratic time

reverse(L,M) :- reverse(L,[],M). %linear time because prepend.
reverse([],M,M).
reverse([X|L],A,M) :- A1 = [X|A], reverse(L,A1,M).

%a tail recursive function calls itself last, cool because prevents stack overflow(but not really in prolog)

%positive(I):- I #>0.
%prime(I):-...

all_pos([]).
all_pos([X|L]) :- positive(X), all_pos(L).

all_prime([]).
all_prime([X|L]) :- is_prime(X), all_prime(L).

%nearly the same all prime and all pos
%So use higher order predicate

%maplist(P,L) true if P(E) true for every element of L

all_pos(L) :- maplist(positive,L).
all_prime(L) :- maplist(is_prime,L).

%predicate that takes a predicate as argument = higher order predicate

greater(I,J) :- I #> J.
inc(I,J) :- J #= I+1.
add(I,J,K) :- K #=I+J.
%foo(L). % true is 7 is greater than every element of L


foo(L) :- maplist(greater(7),L). %second parameter is element of L

%all_dif every element are different from each other
all_dif([]).
all_dif([X|L]) :- maplist(dif(X),L),all_dif(L).

%maplist(P,L,M) -  true if L and M have the same length and P(E,F) is true for corresponing elements E,F in L,M.

%maplist(inc,L,M). M is L inc for every pair [L,M] 
%works in every direction, pure core of the langage

%maplist(add(7),[3,4,5],L) 
%L = [10,11,12]

num(1,one).
num(2,two).
num(3,three).
%maplist(num,[1,2,1],L) -> L = [one,two,one]

%maplist(P,L,M,N) true if P(E,F,G) is true 

vector(2.0,3.1,4.1).
%add vectors V,W
add_vec_vec(V,W,X) :- maplist(fadd,V,W,X).

fadd(X,Y,Z) :- {Z=X+Y}.

%high order predicate , predicate always first argument.

foldl(P,L,V0,V). %V0 starting value V result P takes 3 arguments
sum(L,S):- fodl(add,L,0,S).
%fodl calls P on each elements but with the previous value obtained (like an accumulator)
%fodl l because adding in left associative way

reverse(L,M) :- prepend(X,L,[X|L]).
reverse(L,M) :- foldl(prepend,L,[],M).

%call(add,3,4,X). X=7.
%partialy applied call(add(7),10,X), X =17

maplist(_,[]). %vacuously true
maplist(P,[X|L]) :- call(P,X),maplist(P,L).

maplist(_,[],[]). %vacuously true
maplist(P,[X|L],[Y|M]) :- call(P,X,Y),maplist(P,L,M).

%ex write foldl using call

%graph representation
%temporal
edge(a,b).
edge(a,c).
edge(b,d).

%or
%spatial
%[a->[b,c],b->[d],c->[d,e]]. adjacency list representatione

%we use temporal because we want state search

%depht search first predicate
%dfs(start,goal,path)
%dfs(a,f,[a,c,e,f]).

dfs0(Start, Start, [Start]).
dfs0(Start,Goal, [Start|Path]) :-
    edge(Start,V),
    dfs0(V,Goal,Path).

dfs(Start,Goal, Path) :- dfs(Start,Goal,[Start],Path). %[Start] visited set

dfs(Start,Start,_,[Start]).
dfs(Start,Goal,Visited, [Start|Path] ):-
    edge(Start,V), maplist(dif(V),Visited),
    dfs(V,Goal,[V|Visited],Path).
%dfs bad not shortest path possible

%iterative deepeneing do a dfs with dfs with depth first search 1 then 2,3,4..

iter_deep(Start,Goal,Path):-
    length(P,_), % length(P,_) =gives back P of size 1 then 2 then 3...
    dfs(Start,Goal,P).