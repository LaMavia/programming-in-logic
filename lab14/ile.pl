% ile(+P, -K)
ile(P, _) :-
  clearFlag,
  call(P),
  incFlag,
  fail.

ile(_, K) :-
  getFlag(K).



% setFlag/1 (lub /2), getFlag/1, incFlag/0
setFlag(X) :-
  clearFlag,
  asserta(f(X)).

getFlag(X) :-
  (  f(X) 
  -> true
  ;  X=0
  ).

incFlag :-
  getFlag(X),
  X1 is X + 1,
  setFlag(X1).

clearFlag :-
  retractall(f(_)).

rel(a, b).
rel(a, c).
rel(b, c).



ileFind(P, K) :-
  findall(_, P, S),
  length(S, K).
