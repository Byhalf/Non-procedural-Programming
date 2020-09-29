?- use_module(library(clpfd)).
?- use_module(library(clpr)).

% helper predicate
prepend(L, X, [X | L]).

% if goal is at the head of the queue, return it
bfs(_, [[Goal | Rest] | _], _, Goal, [Goal | Rest]).

% main recursive predicate: bfs(+Succ, +Queue, +Visited, +Goal, -Solution)
bfs(Succ, [[State | Path] | Queue], Visited, Goal, Solution) :-
    findall(X, call(Succ, State, X), Next),    % find all neighboring states
    subtract(Next, Visited, Next1),            % remove already-visited states
    maplist(prepend([State | Path]), Next1, Next2), % prepend each state to path
    append(Queue, Next2, Queue2),              % add all new states to queue
    append(Next1, Visited, Visited1),          % add all new states to visited set
    bfs(Succ, Queue2, Visited1, Goal, Solution).   % recurse

% top-level predicate: bfs(+Succ, Start, +Goal, -Solution)
bfs(Succ, Start, Goal, Solution) :-
    bfs(Succ, [[Start]], [Start], Goal, Solution1),

    reverse(Solution1, Solution).

% t(nil, 1, t(nil, 1, t(nil, 1, nil)))
isTree(nil).
isTree(t(L,_,R)) :- isTree(L),isTree(R).


% t(nil, 1, t(nil, 3, t(nil, 5, nil)))

%treeSum(T,N).
treeSum(nil,0).
treeSum(t(L,X,R), Res) :- 
    treeSum(L,X1),
    treeSum(R,X2),
    Res #= X2 + X1 + X.

succ(t(L,X,R),T2) :- isTree(t(L,X,R)), T2 = L.
succ(t(L,X,R),T2) :- isTree(t(L,X,R)), T2 = R.


% t(nil, 1, t(nil, 3, t(nil, 5, nil)))
% [3,5] leaves(t(nil, 1, t(nil, 3, t(nil, 5, nil))),[3,5])
leaves(T,[]) :- isTree(T).
leaves(T,[S1|S]) :- 
    isTree(T), 
    bfs(succ, T, t(nil,S1,nil), _),
    leaves(T,S).


is_in(T,E) :- bfs(succ, T, t(nil,E,nil), _).

leaves2(T,[]) :- isTree(T).
leaves2(T,S) :- 
    isTree(T), 
    maplist(is_in(T),S).
    
% leaves not very efficient I think should "remenber" end nodes already explored
%children_values(t(t(nil,1,nil),1,t(nil,1,nil)),C).

children_values(t(L,_,_),T2) :-  L=t(_,X1,_), T2 = X1.
children_values(t(_,_,R),T2) :-  R=t(_,X1,_), T2 = X1.

%symmetrical(t(t(nil,1,nil),1,t(nil,1,nil))).
%works only one way
symmetrical(nil).
symmetrical(t(L,_,R)) :- 
    same_length(T1, T2),
    findall(T1, call(children_values,L,T1), L1), %find all might be overkill for binary tree
    findall(T2, call(children_values,R,T2), R2),
    reverse(L1,R2),
    symmetrical(L),
    symmetrical(R).

