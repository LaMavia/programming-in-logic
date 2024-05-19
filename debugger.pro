:- dynamic zuzuFlag/2, get_flag/3, set_flag/2, do_goal/1, do_body/1, do_body/3, scal/3, system/1.

zuzuFlag(mode, [call,exit,redo,fail,exception]).
zuzuFlag(call_id, 1).



% get_flag(+Name, +Default, -Value)
get_flag(Id, Default, Val) :-
  ( zuzuFlag(Id, Val) -> true
  ; Val = Default
  ).

% set_flag(+Name, +Value)
set_flag(Id, Val) :-
  retractall(zuzuFlag(Id, _)),
  asserta(zuzuFlag(Id, Val)).



cleanup :-
  retractall(zuzuFlag(_, _)).



% do_goal(+ Goal)
do_goal(Goal) :- system(Goal), !, call(Goal).
do_goal(Goal) :- 
  clause(Goal, Body),
  do_body(Body, AfterCut, HadCut),
  (
    HadCut = yes,
    !,
    do_body(AfterCut)
  ;
    HadCut = no
  ).



% do_body(+ Body)
do_body(Body) :- 
  do_body(Body, AfterCut, HadCut),
  (
    HadCut = yes,
    !,
    do_body(AfterCut)
  ;
    HadCut = no
  ).



% do_body(+ Body, -AfterCut, -HadCut)
do_body((!, AfterCut), AfterCut, yes) :- !.

do_body(((Disj1; _Disj2), Body), AfterCut, HadCut) :-
  do_body(Disj1, After, HadCutDisj),
  (
    HadCutDisj = yes,
    !,
    HadCut = yes,
    scal(After, Body, AfterCut)
  ;
    HadCutDisj = no,
    do_body(Body, AfterCut, HadCut)
  ).

do_body(((_Disj1; Disj2), Body), AfterCut, HadCut) :-  
  !,
  do_body(Disj2, After, HadCutDisj),
  (
    HadCutDisj = yes,
    !,
    HadCut = yes,
    scal(After, Body, AfterCut)
  ;
    HadCutDisj = no,
    do_body(Body, AfterCut, HadCut)
  ).



do_body(((C -> T), Body), AfterCut, HadCut) :-
  !,
  do_body((C, !, T; Body), AfterCut, HadCut).



do_body((Goal, Body ), AfterCut, HadCut) :- 
  !,
  do_goal(Goal),
  do_body(Body, AfterCut, HadCut).

do_body((Disj1; _Disj2), AfterCut, HadCut) :-
  do_body(Disj1, AfterCut, HadCut).

do_body((_Disj1; Disj2), AfterCut, HadCut) :- 
  !,
  do_body(Disj2, AfterCut, HadCut).

do_body(!, true, yes) :-  !.
do_body(Goal, true, no) :-  do_goal(Goal).



% scal(Conj1, Conj2, Conj1 o Conj2)
scal((A, B), C, (A, BC)) :-  !, scal(B, C, BC).
scal(A, B, (A, B)).


% system(+ Atom)
system(P) :-  predicate_property(P, built_in).
