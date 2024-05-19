:- dynamic do_goal/1, do_body/1, do_body/3.

do_goal(G) :- system(G), !, call(G).


do_goal(G) :- 
  clause(G, Body),
  do_body(Body, HadCut, AfterCut),
  ( HadCut = yes, !, do_body(AfterCut)
  ; HadCut = no
  ).

% do_body(+B)
do_body(B) :-
  do_body(B, HadCut, AfterCut),
  ( HadCut = yes, !, do_body(AfterCut)
  ; HadCut = no
  ).

% do_body(+B, -HadCut, -AfterCut)
do_body((!, B), yes, B) :- !. % bo uzgodni się z (A, B)

do_body(((D1; _D2), B), HadCut, AfterCut) :-
  do_body(D1, HadCut2, AfterCut2),
  ( HadCut2 = yes, !, HadCut = yes, concat(AfterCut2, B, AfterCut) % = (AfterCut2, B)) jest źle, bo AfterCut2 może być listą przecinkową
  ; HadCut = no, do_body(B, HadCut, AfterCut)
  ),
  do_body(B).

do_body(((_D1; D2), B), HadCut, AfterCut) :-
  !, % zatwierdza, że pierwszym członem `,` jest `;`
  do_body(D2, HadCut2, AfterCut2),
  ( HadCut2 = yes, !, HadCut = yes, concat(AfterCut2, B, AfterCut)
  ; HadCut = no
  ),
  do_body(B).

do_body((A, B), HadCut, AfterCut) :- 
  !,
  do_goal(A),
  do_body(B, HadCut, AfterCut).

do_body((D1; _D2), HadCut, AfterCut) :-
  do_body(D1, HadCut, AfterCut).

do_body((_D1; D2), HadCut, AfterCut) :-
  !,
  do_body(D2, HadCut, AfterCut).

do_body(!, yes, true) :- !. % zatwierdza wybór

do_body(true, no, true) :- !.

do_body(A, no, true) :- do_goal(A).


% system(+ Atom)

system(P) :-  predicate_property(P, built_in).
