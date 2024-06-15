:- set_prolog_flag(double_quotes, codes).

% nil | tree(l, v, r)

% infix(+Tree, ?L)
infix(T, L) :- infix(T, [], L).
infix(nil, L, L).
infix(tree(LT, V, RT), U, L) :-
  infix(RT, U, U1),
  infix(LT, [V | U1], L).



infixG(nil) --> [].
infixG(tree(LT, V, RT)) -->
  infixG(LT), [V], infixG(RT).



% postfix(+Tree, ?L)
postfix(T, L) :- postfix(T, [], L).
postfix(nil, L, L).
postfix(tree(LT, V, RT), U, L) :-
  postfix(RT, [V|U], U1),
  postfix(LT, U1, L).


t1(tree(
  tree(
    nil, 
    1, 
    nil
  ), 
  2, 
  tree(
    tree(nil, 3, nil),
    4,
    tree(nil, 5, nil)
))
).
