:- dynamic zuzu_debugger_flag/2, zuzu_get_flag/3
         , zuzu_set_flag/2, do_goal/1
         , do_body/1, do_body/3
         , scal/3, system/1
         , do_goal_impl/1, emit_event/2
         , write_debugger_line/2.



% zuzu_leash(+Mode)  
zuzu_leash(Mode) :-
  ( Mode = all  -> Modes = [call,exit,redo,fail,exception]
  ; Mode = half -> Modes = [call,redo,exception]
  ; Mode = off  -> Modes = []
  ;                Modes = Mode
  ),
  zuzu_set_flag(modes, Modes).



% do_goal(+ Goal)
do_goal(Goal) :-
    zuzu_get_flag(call_id, CallId),
    NextCallId is CallId + 1,
    zuzu_set_flag(call_id, NextCallId),
    emit_event(call, Goal, CallId),
    (
       do_goal_impl(Goal) 
    -> emit_event(exit, Goal, CallId)
    ;  emit_event(fail, Goal, CallId)
    ,  fail
    ).

% do_goal_impl(+Goal)
do_goal_impl(Goal) :- system(Goal), !, call(Goal).
do_goal_impl(Goal) :- 
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



% do_body(+Body, -AfterCut, -HadCut)
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
  do_body(((C, !, T; fail), Body), AfterCut, HadCut).

do_body( ((C -> T; E), Body), AfterCut, HadCut) :-
  !,
  do_body( 
    (
     (
       C, !, T
     ; E
     ), 
     Body
    ), 
    AfterCut, 
    HadCut
  ).



do_body((Goal, Body), AfterCut, HadCut) :- 
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



% port_display(+PortName, -DisplayValue)
port_display(call, "Call").
port_display(exit, "Exit").
port_display(redo, "Redo").
port_display(fail, "Fail").
port_display(exception, "Exception").



% zuzu_debugger_flag(+Key, +Value)
zuzu_debugger_flag(modes, [call,exit,redo,fail,exception]).
zuzu_debugger_flag(call_id, 1).



% zuzu_get_flag(+Name, +Default, -Value)
% Only allows for predefined flags.
zuzu_get_flag(Id, Val) :-
  zuzu_debugger_flag(Id, Val) -> true.



% zuzu_set_flag(+Name, +Value)
zuzu_set_flag(Id, Val) :-
  retractall(zuzu_debugger_flag(Id, _)),
  asserta(zuzu_debugger_flag(Id, Val)).



% emit_event(+Event, +Term, +CallId)
emit_event(Event, Term, CallId) :-
  zuzu_get_flag(modes, TrackedPorts),
  (
    member(Event, TrackedPorts) 
  -> write_debugger_line(Event, Term, CallId) 
  ; true
  ).



% write_debugger_line(+Event, +Term, +CallId)
write_debugger_line(Event, Term, CallId) :-
  port_display(Event, PortDisplay),
  format(user_error, "    ~d   ~s: ~W~n", [CallId, PortDisplay, Term, []]).
