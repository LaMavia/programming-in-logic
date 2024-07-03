% Zuzanna Surowiec 438730
%
% Debugger uruchamiamy za pomocą predykatu z_trace(:Goal).
% Porty można zmieniać za pomocą predykatu z_leash(+Mode),
% który akceptuje [all, half, off] oraz własną listę portów.
% Port `redo` nie jest poprawnie obsługiwany.
% Interakcja z debuggerem prowadzona jest przez wczytywanie termów.
% Obsługiwane polecenia to:
% 1. c. - creep
% 2. s. - skip
% 3. n. - no trace
% 4. a. - abort
% 5. i. - ignore (uznaje obecny goal za spełniony)
% 6. f. - fail (uznaje obecny goal za niespełniony)
% 7. b. - break
%
:- [library(lists)].
:- dynamic z_debugger_flag/2, z_get_flag/3
         , z_set_flag/2, do_goal/1
         , do_body/1, do_body/3
         , scal/3, system/1
         , do_goal_impl/1, emit_event/2
         , write_debugger_line/2
         , myTrace/1, z_leash/1.




% z_trace(:Goal)
z_trace(Goal) :- 
  z_set_flag(ctx, ctx(c, 0, 0, no)),
  z_set_flag(do_trace, yes),
  z_set_flag(call_iota, 0),
  catch((   
      do_goal(Goal)
  *->  true
  ;   \+ z_do_trace
  *->  true
  ;   fail
  ), _, true).


% z_leash(+Mode)  
z_leash(Mode) :-
  ( Mode = all  -> Ports = [call,exit,redo,fail,exception]
  ; Mode = half -> Ports = [call,redo,exception]
  ; Mode = off  -> Ports = []
  ;                Ports = Mode
  ),
  z_set_flag(ports, Ports).

% do_goal(:Goal)
do_goal(Goal) :-
  z_do_trace,
  !,
  with_ctx((
    z_ctx_inc_depth,
    z_ctx_inc_cid,
    catch(
      (
          do_goal_impl(Goal)
      *-> z_do_trace,
          emit_event(exit, Goal)
      ;   z_do_trace
      *-> emit_event(fail, Goal),  
          fail
      ;   true
      ), 
      error(Reason, Rest), 
      (
        format("~p\n", [Reason]),
        emit_event(exception, Goal),
        throw(error(Reason, Rest))
      )
    )
  )).

do_goal(Goal) :-
  \+ z_do_trace,
  call(Goal).


% do_goal_impl(:Goal)
do_goal_impl(Goal) :- system(Goal), !, call(Goal).

do_goal_impl(Goal) :- 
  clause(Goal, Body),
  format("~q\n", [Body]),
  emit_event(call, Goal),
  do_body(Body, AfterCut, HadCut),
  (
    HadCut = yes,
    !,
    do_body(AfterCut)
  ;
    HadCut = no
  ).



% port_display(+PortName, -DisplayValue)
port_display(call, "Call").
port_display(exit, "Exit").
port_display(redo, "Redo").
port_display(fail, "Fail").
port_display(exception, "Exception").



% z_debugger_flag(+Key, +Value)
z_debugger_flag(ports, [call,exit,redo,fail,exception]).



% z_do_trace/0
z_do_trace :-
  z_ctx_get(mode, M),
  (   member(M, [s, n])
  ->  fail
  ;   z_get_flag(do_trace, yes)
  ).



% z_ctx_index(+Name, -Index)
z_ctx_index(mode, 1).
z_ctx_index(depth, 2).
z_ctx_index(call_id, 3).
z_ctx_index(in_redo, 4).



% z_get_flag(+Name, +Default, -Value)
% Only allows for predefined flags.
z_get_flag(Id, Val) :-
  z_debugger_flag(Id, Val) -> true.



% z_set_flag(+Name, +Value)
z_set_flag(Id, Val) :-
  retractall(z_debugger_flag(Id, _)),
  asserta(z_debugger_flag(Id, Val)).



% z_get_ctx(-Ctx)
% Gets the current context.
z_get_ctx(Ctx) :-
  z_get_flag(ctx, Ctx).



% z_set_ctx(-Ctx)
% Sets the current context.
z_set_ctx(Ctx) :-
  z_set_flag(ctx, Ctx).



% z_ctx_get(+L, ?R)
% Gets the value of field L from the current context.
z_ctx_get(L, R) :-
  z_ctx_index(L, Index),
  z_get_ctx(Ctx),
  Ctx =.. [_ | Args],
  nth1(Index, Args, R).



% z_ctx_set(+L, +R)
% Sets the value of the field L in the current context.
z_ctx_set(L, R) :-
  z_ctx_index(L, I),
  z_get_ctx(Ctx0),
  Ctx0 =.. [F | Args0],
  set_nth1(Args0, I, R, Args1),
  Ctx1 =.. [F | Args1],
  z_set_ctx(Ctx1).



% z_ctx_inc_cid/0
% Increments call_iota by 1, 
% and updates call_id to 
% the new value of call_iota.
z_ctx_inc_cid :-
  z_get_flag(call_iota, I),
  I1 is I + 1,
  z_set_flag(call_iota, I1),
  z_ctx_set(call_id, I1).



% z_ctx_inc_depth
% Increments depth by 1.
z_ctx_inc_depth :-
  z_ctx_get(depth, D),
  D1 is D + 1,
  z_ctx_set(depth, D1).



% z_ctx_reset_redo/0
% Sets in_redo to `no`
z_ctx_reset_redo :-
  z_ctx_set(in_redo, no).



% with_ctx(:G/0)
% Calls G in a copy of the current context.
% Restores the current context. 
with_ctx(G) :-
  z_get_ctx(Ctx),
  with_ctx(Ctx, G).


% with_ctx(+Ctx, :G/0)
% Calls G in the context Ctx.
% Restores the current context. 
with_ctx(Ctx, G) :- with_ctx(Ctx, G, _).



% with_ctx(+Ctx, :G/0, -Ctx)
% Calls G in the context Ctx, returning a modified context.
% Restores the current context. 
with_ctx(Ctx, G, Ctx1) :-
  z_get_ctx(Ctx0),
  z_set_ctx(Ctx),
  z_ctx_reset_redo,
  AfterCall = (z_get_ctx(Ctx1), z_set_ctx(Ctx0)),
  (   call(G)
  *-> call(AfterCall)
  ;   call(AfterCall),
      fail
  ).



emit_event(Event, Term) :-
  z_get_flag(ports, TrackedPorts),
  (
     member(Event, TrackedPorts)
  -> write_debugger_line(Event, Term),
     handle_input(Event)
  ;  true
  ).



% write_debugger_line(+Event, +Term)
write_debugger_line(Event, Term) :-
  z_ctx_get(call_id, CallId),
  z_ctx_get(depth, Depth),
  port_display(Event, PortDisplay),
  format(user_error, "   ~d  ~d   ~s: ~p ? ", [CallId, Depth, PortDisplay, Term]).



% set_nth1(+List, +N, +Elem, -List)
set_nth1([_|XS], 1, Y, [Y|XS]) :- !.
set_nth1([X|XS], N, Y, [X|YS]) :- 
  N > 1,
  N1 is N - 1,
  set_nth1(XS, N1, Y, YS).



% handle_input(+Event)
handle_input(Event) :-
  read(C),
  handle_input(C, Event).



% handle_input(+Char, +Event)
handle_input('c', _) :- z_ctx_set(mode, c).
handle_input('i', _) :- z_ctx_set(mode, i).
handle_input('n', _) :- z_set_flag(do_trace, no).
handle_input('a', _) :- abort. 
handle_input('e', _) :- halt.
handle_input('b', _) :- break.
handle_input('s', Event) :-
  (   
      member(Event, [call, redo])
  ->  z_ctx_set(mode, s)
  ;   z_ctx_set(mode, c)
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
     ( C, !, T; E), 
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


q(a).
q(A) :- A \= b.

my_member(X, [X|_]).
my_member(X, [_|XS]) :- my_member(X, XS).
