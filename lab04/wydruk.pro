% wydruk(+P/K)
wydruk(P/K) :-
  functor(Head, P, K),
  clause(Head, Body),
   portray_clause(( Head :- Body )), fail.
  % format("~p :- ~p.\n", [Head, Body]), fail.

wydruk(_).
