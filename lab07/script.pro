:- dynamic intersect/2, mem/2.

intersect(X,Y) :-  mem(Z,X), mem(Z,Y).

mem(X, [X|_]).
mem(X, [_|L]) :-  mem(X,L).


