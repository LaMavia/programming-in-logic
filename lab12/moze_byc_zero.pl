:- set_prolog_flag(double_quotes, codes).

% moze_byc_zero(L) :-
%   phrase(maybe_zero(0), L).

maybe_zero(W) --> [L], {integer(L)}, !, reszta(L, W).
maybe_zero(W) --> ['('], maybe_zero(W1), [')'], reszta(W1, W).

reszta(W, W) --> [].
reszta(LW, W) --> 
  [Op], 
  { isOp(Op) }, 
  !, 
  maybe_zero(RW), 
  { doOp(Op, LW, RW, LW1) }, 
  reszta(LW1, W).

isOp(Op) :- member(Op, [*, +, -]).
doOp(Op, L, R, W) :- 
  E =.. [Op, L, R],
  W is E.


wartosc(L, W) :- phrase(maybe_zero(W), L, []).


maybe_zero(W, R) --> [L], {integer(L)}, !, reszta(L, W, L, R).
maybe_zero(W, R) --> 
  ['('], 
  maybe_zero(W1, R1), 
  [')'], 
  reszta(W1, W, R1, R).

reszta(W, W, R, R) --> [].
reszta(LW, W, LR, R) --> 
  [Op], 
  { isOp(Op) }, 
  !, 
  maybe_zero(RW, RR), 
  { doOp(Op, LW, RW, LW1) }, 
  { Rep =.. [Op, LR, RR] },
  reszta(LW1, W, Rep, R).


wartosc(L, W, R) :-
  phrase(maybe_zero(W, R), L, []).
  

