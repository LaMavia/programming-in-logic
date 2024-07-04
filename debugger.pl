% Zuzanna Surowiec 438730
%
% Debugger uruchamiamy za pomocą predykatu myTrace(:Goal).
% Porty można zmieniać za pomocą predykatu z_leash(+Mode),
% który akceptuje [all, half, off] oraz własną listę portów.
% Port `redo` nie jest poprawnie obsługiwany.
% Interakcja z debuggerem prowadzona jest przez 
% wczytywanie znaków zatwierdzanych przyciskiem enter.
% Obsługiwane polecenia to:
% 1. c. - creep
% 2. s. - skip
% 3. e. - halt
% 4. a. - abort
%
%
:- [library(lists)].
:- expects_dialect(sicstus).
:- dynamic z_debugger_flag/2.



% z_trace(:Goal)
z_trace(Goal) :- 
  Ctx = ctx(c, 0, 0, no),
  z_set_flag(call_iota, 0),
  prompt(OldPrompt, ?),
  if(
    do_goal(Goal, Ctx), 
    prompt(_, OldPrompt), 
    prompt(_, OldPrompt)
  ).


% z_leash(+Mode)  
z_leash(Mode) :-
  ( Mode = all  -> Ports = [call,exit,redo,fail,exception]
  ; Mode = half -> Ports = [call,redo,exception]
  ; Mode = off  -> Ports = []
  ;                Ports = Mode
  ),
  z_set_flag(ports, Ports).

% do_goal(:Goal, +Ctx)
do_goal(Goal, Ctx) :-
  (
      Goal = true
  ->  Ctx = Ctx2
  ;   z_ctx_inc_depth(Ctx, Ctx1),
      z_ctx_inc_cid(Ctx1, Ctx2)
  ),
  catch(
    if(
      do_goal_impl(Goal, Ctx2),
      emit_event(exit, Goal, Ctx2, _),
      ( 
        emit_event(fail, Goal, Ctx2, _),  
        fail
      )
    ), 
    error(Reason, _), 
    (
      format("\x1b[1;31m~p\x1b[1;0m (~q)\n", [Reason, Ctx]),
      emit_event(exception, Goal, Ctx2, _),
      fail
    )
  ).



% do_goal_impl(:Goal, Ctx)
do_goal_impl(Goal, Ctx) :- 
  system(Goal), 
  !, 
  emit_event(call, Goal, Ctx, _),
  call(Goal).

do_goal_impl(Goal, Ctx) :- 
  \+ system(Goal),
  clause(Goal, Body),
  format(">>> ~q (~q)\n", [(Goal :- Body), Ctx]),
  emit_event(call, Goal, Ctx, CtxBody),
  do_body(Body, CtxBody, AfterCut, HadCut),
  (
    HadCut = yes,
    !,
    do_body(AfterCut, CtxBody)
  ;
    HadCut = no
  ).



% port_display(+PortName, -DisplayValue)
port_display(call, "\x1b[1;32mCall\x1b[1;0m").
port_display(exit, "\x1b[1;32mExit\x1b[1;0m").
port_display(redo, "\x1b[1;33mRedo\x1b[1;0m").
port_display(fail, "\x1b[1;31mFail\x1b[1;0m").
port_display(exception, "\x1b[1;31mException\x1b[1;0m").



% z_debugger_flag(+Key, +Value)
z_debugger_flag(ports, [call,exit,redo,fail,exception]).



% z_do_trace(+Ctx)
z_do_trace(Ctx) :-
  z_ctx_get(Ctx, mode, M),
  (   member(M, [s, n])
  ->  fail
  ;   true
  ).



% z_ctx_index(+Name, -Index)
z_ctx_index(mode, 1).
z_ctx_index(depth, 2).
z_ctx_index(call_id, 3).



% z_get_flag(+Name, +Default, -Value)
% Only allows for predefined flags.
z_get_flag(Id, Val) :-
  z_debugger_flag(Id, Val) -> true.



% z_set_flag(+Name, +Value)
z_set_flag(Id, Val) :-
  retractall(z_debugger_flag(Id, _)),
  asserta(z_debugger_flag(Id, Val)).



% z_ctx_get(+Ctx, +L, ?R)
% Gets the value of field L from the current context.
z_ctx_get(Ctx, L, R) :-
  ground(Ctx),
  z_ctx_index(L, Index),
  Ctx =.. [_ | Args],
  nth1(Index, Args, R).



% z_ctx_set(+Ctx, +L, +R, -Ctx)
% Sets the value of the field L in the current context.
z_ctx_set(Ctx, L, R, Ctx1) :-
  ground(Ctx),
  z_ctx_index(L, I),
  Ctx =.. [F | Args0],
  set_nth1(Args0, I, R, Args1),
  Ctx1 =.. [F | Args1].



% z_ctx_inc_cid/0
% Increments call_iota by 1, 
% and updates call_id to 
% the new value of call_iota.
z_ctx_inc_cid(Ctx, Ctx1) :-
  z_get_flag(call_iota, I),
  I1 is I + 1,
  z_set_flag(call_iota, I1),
  z_ctx_set(Ctx, call_id, I1, Ctx1).



% z_ctx_inc_depth(+Ctx, -Ctx)
% Increments depth by 1.
z_ctx_inc_depth(Ctx, Ctx1) :-
  z_ctx_get(Ctx, depth, D),
  D1 is D + 1,
  z_ctx_set(Ctx, depth, D1, Ctx1).



% z_ctx_reset_redo(+Ctx, -Ctx)
% Sets in_redo to `no`
z_ctx_reset_redo(Ctx, Ctx1) :-
  z_ctx_set(Ctx, in_redo, no, Ctx1).



emit_event(Event, Term, Ctx, Ctx1) :-
  format("[~q] Term: ~q, Ctx: ~q\n", [Event, Term, Ctx]),
  Term \= true,
  !,
  z_get_flag(ports, TrackedPorts),
  (
     member(Event, TrackedPorts),
     z_do_trace(Ctx)
  -> write_debugger_line(Event, Term, Ctx),
     handle_input(Ctx, Event, Ctx1)
  ;  Ctx1 = Ctx
  ).

emit_event(_, true, Ctx, Ctx).


% write_debugger_line(+Event, +Term, +Ctx)
write_debugger_line(Event, Term, Ctx) :-
  z_ctx_get(Ctx, call_id, CallId),
  z_ctx_get(Ctx, depth, Depth),
  port_display(Event, PortDisplay),
  format(user_error, "   ~d  ~d   ~s: ~p (~q) ? ", [CallId, Depth, PortDisplay, Term, Ctx]).



% set_nth1(+List, +N, +Elem, -List)
set_nth1([_|XS], 1, Y, [Y|XS]) :- !.
set_nth1([X|XS], N, Y, [X|YS]) :- 
  N > 1,
  N1 is N - 1,
  set_nth1(XS, N1, Y, YS).



% handle_input(+Ctx, +Event, -Ctx)
handle_input(Ctx, Event, Ctx1) :-
  get_code(CCode),
  skip('\n'),
  !,
  char_code(C, CCode),
  handle_input(C, Event, Ctx, Ctx1).



% handle_input(+Char, +Event)
handle_input('c', _, Ctx, Ctx1) :- z_ctx_set(Ctx, mode, c, Ctx1).
handle_input('a', _, Ctx, Ctx) :- abort. 
handle_input('e', _, Ctx, Ctx) :- halt.
handle_input('s', Event, Ctx, Ctx1) :-
  (   
      member(Event, [call, redo])
  ->  z_ctx_set(Ctx, mode, s, Ctx1)
  ;   z_ctx_set(Ctx, mode, c, Ctx1)
  ).



% do_body(+ Body, +Ctx)
do_body(Body, Ctx) :- 
  do_body(Body, Ctx, AfterCut, HadCut),
  (
    HadCut = yes,
    !,
    do_body(AfterCut, Ctx)
  ;
    HadCut = no
  ).



% do_body(+Body, +Ctx, -AfterCut, -HadCut)
do_body((!, AfterCut), _, AfterCut, yes) :- !.

do_body(((Disj1; _Disj2), Body), Ctx, AfterCut, HadCut) :-
  do_body(Disj1, Ctx, After, HadCutDisj),
  (
    HadCutDisj = yes,
    !,
    HadCut = yes,
    scal(After, Body, AfterCut)
  ;
    HadCutDisj = no,
    do_body(Body, Ctx, AfterCut, HadCut)
  ).

do_body(((_Disj1; Disj2), Body), Ctx, AfterCut, HadCut) :-  
  !,
  do_body(Disj2, Ctx, After, HadCutDisj),
  (
    HadCutDisj = yes,
    !,
    HadCut = yes,
    scal(After, Body, AfterCut)
  ;
    HadCutDisj = no,
    do_body(Body, Ctx, AfterCut, HadCut)
  ).



do_body(((C -> T), Body), Ctx, AfterCut, HadCut) :-
  !,
  do_body(((C, !, T), Body), Ctx, AfterCut, HadCut).

do_body( ((C -> T; E), Body), Ctx, AfterCut, HadCut) :-
  !,
  do_body( 
    (
     ( C, !, T; E), 
     Body
    ), 
    Ctx,
    AfterCut, 
    HadCut
  ).



do_body((Goal, Body), Ctx, AfterCut, HadCut) :- 
  !,
  do_goal(Goal, Ctx),
  do_body(Body, Ctx, AfterCut, HadCut).

do_body((Disj1; _Disj2), Ctx, AfterCut, HadCut) :-
  do_body(Disj1, Ctx, AfterCut, HadCut).

do_body((_Disj1; Disj2), Ctx, AfterCut, HadCut) :- 
  !,
  do_body(Disj2, Ctx, AfterCut, HadCut).

do_body(!, _, true, yes) :-  !.

do_body(Goal, Ctx, true, no) :-  do_goal(Goal, Ctx).



% scal(Conj1, Conj2, Conj1 o Conj2)
scal((A, B), C, (A, BC)) :-  !, scal(B, C, BC).
scal(A, B, (A, B)).



% system(+ Atom)
system(P) :-  predicate_property(P, built_in).

my_member(X, [X|_]).
my_member(X, [_|XS]) :- my_member(X, XS).

q(_).
q(A) :- A > 1, !, fail.
q(_) :- format("Bye:/\n", []).
