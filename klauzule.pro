:- dynamic mmember/2.

mmember(X, [X|_]).
mmember(X, [_, XS]) :- mmember(X, XS).

clauses(G, N) :-
  asserta(c(0)),
  clauses(G),
  c(N),
  retract(c(_)).

clauses(G) :-
  clause(G, _),
  c(N),
  N1 is N+1,
  retractall(c(_)),
  asserta(c(N1)),
  fail.
clauses(_).

klauzule(G, NS) :-
  clauses(N),
  klauzule(NS, G, 1, N).

klauzule([], _, N, N).

