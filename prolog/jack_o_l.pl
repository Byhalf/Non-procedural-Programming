?- use_module(library(clpfd)).
?- use_module(library(clpr)).



swap(out,lit).
swap(lit,out).


%2nd attempt %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flip3(L,T) :-
    same_length(L, T),
    length(F, 3),
    append([A, F, B], T),
    maplist(swap, F, G),
    append([A, G, B], L).

flip3(L,T) :-
    same_length(L, T),
    append([[X], Shared, [Y, Z]], T),
    maplist(swap, [X, Y, Z], [X1, Y1, Z1]),
    append([[X1], Shared, [Y1, Z1]], L).


flip3(L,T) :-
    same_length(L, T),
    append([[X,Y], Shared, [Z]], T),
    maplist(swap, [X, Y, Z], [X1, Y1, Z1]),
    append([[X1,Y1], Shared, [Z1]], L).




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

unlit([out]).
unlit([out|L]) :- unlit(L).

pumpkin(Initial,Path) :-
    same_length(Initial, Goal),
    unlit(Goal),
    bfs(flip3,Initial,Goal,Path).
