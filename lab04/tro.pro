% tro(+Nazwa/Arność)
% wypisanie klauzul podanego predykatu, w których jest rekurencja ogonowa

:- dynamic lst/2.
lst([ H | [] ], H).


tro(P/K) :-
  functor(Head, P, K),
  clause(Head, Body),
  has_tail_call(Body, P/K),
  portray_clause(( Head :- Body )).

has_tail_call(','(_, T), P/K) :-
  !,
  has_tail_call(T, P/K).

has_tail_call(Term, P/K) :-
  functor(Term, A, N),
  A/N \= ','/2,        % Term \= (_, _) zły,
                       % bo nieprawidłowe użycie negacji.
  functor(Term, P, K).



