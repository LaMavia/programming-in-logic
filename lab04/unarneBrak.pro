% :- dynamic unarneBrak/1.

% unarneBrak(+P/K)
unarneBrak(P/K) :-
  functor(Head, P, K),
  (clause(Head, true) -> 
    unarne(P/K)
  ; write('NIE')
  ).

