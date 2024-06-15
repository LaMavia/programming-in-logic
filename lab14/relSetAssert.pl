:- dynamic s/2.

rel(a, b).
rel(a, c).
rel(a, d).
rel(b, a).
rel(b, d).

relSet(X, _) :-
  % retractall(s(X, _)), % cleanup
  rel(X, Y),
  (  s(X, A)           % getFlag 
  -> true
  ;  A = []
  ),
  retractall(s(X, _)), % setflag
  asserta(s(X, [Y|A])),
  fail.                % loop

relSet(X, S) :-
  s(X, S),
  retract(s(X, S)).
