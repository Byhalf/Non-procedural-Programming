male(jan).
male(karel).
male(vaclav).
female(anna).
female(eliska).

/*Prolog queries top to down*/

dance(P, Q) :- male(P), female(Q).

drinks(tomas, whiskey).
drinks(sonya, beer).
drinks(lucie, wine).
drinks(radek, beer).
drinks(jarda, beer).

pair(P, Q, D) :- drinks(P, D), drinks(Q, D).

edge(g, h).
edge(g, d).
edge(e, d).
edge(h, f).
edge(e, f).
edge(a, e).
edge(a, b).
edge(b, f).
edge(b, c).
edge(f, c).

path(V, W) :- edge(V, W).
path(V, W) :- edge(V,Z), path(Z,W).



parent(vaclav, anna). parent(vaclav, karel).
parent(karel, jan). parent(karel, eliska).
parent(karel, jan). parent(karel, eliska).

sibling(P,Q) :- dif(P,Q), parent(Q,P), parent(P,Q).
full_sibling(P,Q) :- parent(R,P), parent(P,Q), parent(R,S), parent(S,Q), dif(P,Q), dif(R,S).




color(red). 
color(green).
color(blue).
coloring(A,C,G,P,H,S,U) :- color(A),color(C),color(G),color(P),color(H),color(S),color(U).

person(katka).
person(maria).
person(roman).

different(P, Q, R) :-
  person(P), person(Q), person(R),
  dif(P, Q), dif(Q, R), dif(P, R).
  
solution(Doctor, Lawyer, Teacher, Piano, Flute, Violin) :-
  different(Doctor, Lawyer, Teacher),
  different(Piano, Flute, Violin),
  dif(maria, Doctor),
  Lawyer = Piano,
  dif(maria, Teacher),
  Violin = Doctor,
  dif(katka, Doctor).