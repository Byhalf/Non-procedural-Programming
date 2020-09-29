female(emma).
female(stella).
male(tobias).
male(damian).

across(P,Q) :- female(P),female(Q).
across(P,Q) :- male(Q),male(P).

person(P) :- male(P); female(P).



different(P, Q, R) :-
    person(P), person(Q), person(R),
    dif(P, Q), dif(Q, R), dif(P, R).
    


solve(Dumplings, Pasta, Soup, Trout) :-
    different(Dumplings, Pasta, Soup), different(Pasta, Soup, Trout), dif(Dumplings,Trout),
    different(Cider,Beer,Tea), different(Beer,Tea,Wine), dif(Cider,Wine),
    Dumplings = Beer, across(Cider,Trout), Soup=Cider, across(Pasta,Beer), dif(damian,Tea),
    emma = Wine, dif(stella, Dumplings).

%solve(Dumplings, Pasta, Soup, Trout)
