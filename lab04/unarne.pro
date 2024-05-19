% unarne(+P/K)
unarne(P/K) :-
  functor(Head, P, K),
  clause(Head, true),
  portray_clause(Head), fail.

