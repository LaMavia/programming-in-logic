% rekurencja(+Nazwa/Arność)
% wypisanie klauzul podanego predykatu, w których jest rekurencja
% (niekoniecznie ogonowa)

tro(P/K) :-
  functor(Head, P, K),
  clause(Head, Body),
  tro(Body, P/K),
  portray_clause(( Head :- Body )).

tro((H, _), P/K) :-
  functor(H, P, K), !.

tro((_, T), P/K) :-
  !,
  tro(T, P, K).

tro(Term, P/K) :-
  functor(Term, P, K).
