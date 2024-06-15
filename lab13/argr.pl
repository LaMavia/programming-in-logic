:- set_prolog_flag(double_quotes, codes).

ar(P, V) --> 
  [L],
  reszta(L, P, V).

reszta(L, P, V) -->
  [Op],
  {oppri(Op, P1)},
  ar(P2, V1),
  { NL =.. [Op, L, V1] },
  reszta().
reszta(L, _, L) --> [].

