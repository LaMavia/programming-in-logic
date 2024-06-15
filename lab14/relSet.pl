rel(a, b).
rel(a, c).
rel(a, d).

rel(b, d).


% relSet(?X, ?S)
relSet(X, S) :-
  relSet(X, [], S).

relSet(X, YS, ZS) :-
  rel(X, Y),
  \+member(Y, YS),
  !,
  relSet(X, [Y|YS], ZS).

relSet(_, YS, YS).

