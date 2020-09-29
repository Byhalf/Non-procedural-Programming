?- use_module(library(clpfd)).

%wrong
addition(z,J,J).
addition(s(I),J,s(K)) :- addition(I,J,K).

%test(X) :- X in 0..5, label([X]);

multiplication(z,_,z).
multiplication(s(z),X,X).
multiplication(s(X),Y,Z) :- 
        multiplication(X,Y,Z1)
        ,addition(Z1,Y,Z).

factorial(0, 1).
factorial(N, F) :-
        N #> 0,
        N1 #= N - 1,
        factorial(N1, F1),
        F #= N * F1.


