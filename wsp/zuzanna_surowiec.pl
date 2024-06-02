:- op(700, xfx, <>).
:- [library(lists)].
:- dynamic step/8, evalInstr/3, replaceVars/3.

% verify(+N, +File)

verify(N, _) :-
  (\+ integer(N) ; N =< 0),
  !,
  format("Error: parametr ~p powinien byc liczba > 0", [N]),
  fail.

verify(N, File) :-
  N > 0,
  set_prolog_flag(fileerrors, off),
  open(File, read, Stream),
  read(Stream, variables(Vars)),
  read(Stream, arrays(Arrs)),
  read(Stream, program(Instrs)),
  !,                                % program is correctly written
  initState( variables(Vars)
           , arrays(Arrs)
           , program(Instrs)
           , N
           , S
           ),
  (  arg(1, S, []) 
  -> format("Program jest poprawny (bezpieczny).~n", [])
  ;  verify(program(Instrs), S, [], [], _, _, CS, IS) 
  -> printUnsafe(CS, IS)
  ;  format("Program jest poprawny (bezpieczny).~n", [])
  ).

verify(_, File) :-
  format("Error: brak pliku o nazwie - ~s~n", [File]),
  fail.



% verify( +Program, +State, +History, +Moves
%                 , -State, -History, -Moves, -InSection
%       ) 
verify(_, S, H, CS, S, H, CS, IS) :-
  isUnsafe(S, IS),
  !.

verify(_, S, H, CS, S, H, CS, _) :-
  member(S, H),
  !,
  fail.

verify(P, S, H, CS, S1, H1, CS1, IS) :-
  step(P, S, H, CS, _, S0, H0, CS0),
  verify(P, S0, H0, CS0, S1, H1, CS1, IS).



% printUnsafe(+Steps, +InSection)
printUnsafe(CS, IS) :-
  format("Program jest niepoprawny.~nNiepoprawny przeplot:~n", []),
  printSteps(CS),
  format("Procesy w sekcji: ", []),
  printListSep(IS, ", ", ".").



% printSteps(+Steps)
printSteps([]).

printSteps([pair(Pid, L) | XS]) :-
  format("  Proces ~d: ~d~n", [Pid, L]),
  printSteps(XS).



% printListSep(+List, +Separator, +Final)
printListSep([], _, _).

printListSep([X], _, F) :-
  !,
  format("~p~s", [X, F]).

printListSep([X | XS], S, F) :-
  format("~p~s", [X, S]),
  printListSep(XS, S, F).



% state representation:
% state(SectionLines, LenInstrs, PCs, Stack)
%   SectionLines: list of instruction line numbers containing `section`
%   LenInstrs: number of instructions
%   PCs: [pair(Pid, InstructionNumber)]
%   Stack: [pair(Key, Value)]
%             ^ `Id` or an array expression `array(Id, Index)`
%
% For instance: 
%   varaibles([ x ]).
%   arrays([ arr ]).
%   program([
%     assign(x, 1),
%     assign(array(arr, 1), 5)
%   ]).
% 
% , for N=1, has the states:
%   S1 = state(2, [pos(1, 1)], []), H1 = []
%   S2 = state(2, [pos(1, 2)], [v(x, 1)]) H2 = [S1]
%   S3 = state(2, [pos(1, 1)], [v(array(arr, 1), 5), v(x, 1)]) H3 = [S2, S1]
% 
% initState(+Variables, +Arrays, +Program, +N, -StanPoczątkowy)
initState( variables(VarNames)
         , arrays(ArrayNames)
         , program(Instrs)
         , N
         , state(SectionLines, N, L, [], Stack)
         ) :-
  maplist(initVar, VarNames, Vars),
  maplist(initArray(N, 0), ArrayNames, Arrs),
  append([Vars | Arrs], Stack),
  findIncides(Instrs, sekcja, SectionLines),
  length(Instrs, L).


initVar(X, pair(X, 0)).

% initArray(N, I, X, Arr)
initArray(N, I, _, []) :-
  (I >= N ; I < 0),
  !.

initArray(N, I, X, [pair(array(X, I), 0) | Tail]) :-
  I < N,
  I1 is I + 1,
  initArray(N, I1, X, Tail).



% findIncides(+List, +Value, ?Indices)
findIncides(Xs, V, Ixs) :-
  findIncides(Xs, V, 1, Ixs).



% findIncides(+List, +Value, +Index, ?Indices)
findIncides([], _, _, []).
findIncides([X | XS], V, N, LS) :-
  N1 is N + 1,
  ( X = V -> LS = [N | LS1]
  ; LS = LS1
  ),
  findIncides(XS, V, N1, LS1).



% step(+Program, +StanWe, +History   , +CallStack
%     , ?PrId  , -StanWy, -NewHistory, -NewCallStack
%     )

step(P, S, H, CS, Pid, S1, H1, CS1) :-
  arg(2, S, N),
  var(Pid),
  !,
  step_gen(0, N, [P, S, H, CS], [S1, H1, CS1]).

% step(_, _, _, _, Pid, )

step(_, S, H, _, _, _, _, _) :-
  member(S, H),
  !,
  fail.

step( program(Instrs)
    , State
    , History
    , CS
    , Pid
    , state(SIx, N, L, NewPCs, NewStack)
    , [State | History]
    , [pair(Pid, PidPC) | CS]
    ) :-
  \+ member(State, History),
  State = state(SIx, N, L, PCs, Stack),
  integer(Pid),
  Pid >= 0,
  Pid < N,
  assocLookup(PCs, Pid, 1, PidPC),            % get the current pid PC
  valSet(Stack, pid, Pid, EvalStack),         % set the pid var
  nth1(PidPC, Instrs, Instr),                 % get the current instruction
  evalInstr( Instr                            % eval the instruction 
           , state(SIx, N, L, PCs, EvalStack)
           , state(SIx, N, L, PCs1, PEStack)
           ),    
  assocDelete(PEStack, pid, NewStack),
  assocLookup(PCs1, Pid, PidPC, PidPC1),      % get the updated pid
  NewPidPC is (PidPC1 mod L) + 1,             % move to the next instruction
  replaceOrPush(PCs1, Pid, NewPidPC, NewPCs). % update the pid PC 
  


step_gen(Pid, N, _, _) :-
  (Pid < 0 ; Pid >= N),
  !,
  fail.

step_gen(Pid, N, LArgs, RArgs) :-
  Pid >= 0,
  Pid < N,
  append(LArgs, [Pid | RArgs], Args),
  G =.. [step | Args],
  call(G).

step_gen(Pid, N, LArgs, RArgs) :-
  Pid >= 0,
  Pid < N,
  Pid1 is Pid + 1,
  step_gen(Pid1, N, LArgs, RArgs).



% isUnsafe(+State, -PidsInSection)
isUnsafe(state(SIx, N, _, PCs, _), IS) :-
  isUnsafe(SIx, 0, N, PCs, IS),
  length(IS, L),
  L > 1.

isUnsafe(_, Pid, N, _, []) :-
  (Pid < 0 ; Pid >= N),
  !.

isUnsafe(SIx, Pid, N, PCs, IS) :-
  Pid < N,
  assocLookup(PCs, Pid, 1, PidPC),
  ( member(PidPC, SIx) -> IS = [Pid | IS1]
  ; IS = IS1
  ),
  Pid1 is Pid + 1,
  isUnsafe(SIx, Pid1, N, PCs, IS1).



% evalInstr(Instr, Stack, NewStack)
evalInstr( assign(Id, VExpr)
         , state(SIx, N, L, PCs, Stack)
         , state(SIx, N, L, PCs, NewStack)
         ) :-
  valSet(Stack, Id, VExpr, NewStack).

evalInstr( goto(M)
         , state(SIx, N, L, PCs , Stack)
         , state(SIx, N, L, PCs1, Stack)
         ) :-
  M1 is M - 1,
  valLookup(Stack, pid, Pid),
  replaceOrPush(PCs, Pid, M1, PCs1).

evalInstr( condGoto(Cond, M)
         , State
         , NewState
         ) :-
  arg(5, State, Stack),
  replaceVars(Stack, Cond, Cond1),
  ( call(Cond1) -> evalInstr(goto(M), State, NewState)
  ; NewState = State
  ).

evalInstr(sekcja, State, State).



% preprocessProgram(+Program, -NewProgram)
preprocessProgram(program(IS), program(NewIS)) :-
  map(IS, preprocessProgramAux, NewIS).

preprocessProgramAux(I, NI) :-
  I =.. [F | Args],
  map(Args, replaceOperators, NewArgs),
  NI =.. [F | NewArgs].



% Since comparison operators cannot be nested,
% there's no need for recursion.
% replaceOperators(+Expr, -Expr)
replaceOperators((AExpr <> BExpr), (AExpr =\= BExpr)) :- !.
replaceOperators((AExpr = BExpr), (AExpr =:= BExpr)) :- !.
replaceOperators(Expr, Expr).



% replaceVars(+Stack, +Expr, -Expr)
replaceVars(S, array(ArrName, IExpr), array(ArrName, IExpr1)) :-
  nonvar(IExpr),
  !,
  replaceVars(S, IExpr, IExpr1).

replaceVars(S, Expr, Expr1) :-
  nonvar(Expr),
  Expr =.. [F, AExpr, BExpr],
  !,
  replaceVars(S, AExpr, AExpr1),
  replaceVars(S, BExpr, BExpr1),
  Expr1 =.. [F, AExpr1, BExpr1].

replaceVars(_, Expr, Expr) :-
  number(Expr),
  !.

replaceVars(S, VarName, VarValue) :-
  nonvar(VarName),
  functor(VarName, _, 0),
  valLookup(S, VarName, VarValue).



% valLookup(+Stack, +Id, -Value)
valLookup(Stack, array(Id, IExpr), Value) :-
  !,
  Index is IExpr,
  assocLookup(Stack, array(Id, Index), 0, Value).

valLookup(Stack, Id, Value) :-
  functor(Id, F, K),              % make the last cut green 
  F/K \= array/2,
  assocLookup(Stack, Id, 0, Value).



% assocLookup(+Assoc, +Key, +DefaultValue, -Value)
assocLookup([], _, Value, Value).
assocLookup([X | _], Key, _, Value) :-
  X =.. [_, Key, Value],
  !.
assocLookup([X | XS], Key, DefaultValue, Value) :-
  X =.. [_, OtherKey, _], % make the previous cut green
  OtherKey \= Key,
  assocLookup(XS, Key, DefaultValue, Value).



% valSet(+Stack, +Id, +Value, -NewStack)
valSet( Stack
      , array(Id, IExpr)
      , VExpr
      , NewStack
      ) :-
  !,
  replaceVars(Stack, IExpr, IExpr1),
  replaceVars(Stack, VExpr, VExpr1),
  Index is IExpr1,
  Value is VExpr1,
  replaceOrPush(Stack, array(Id, Index), Value, NewStack).

valSet( Stack
      , Id
      , VExpr
      , NewStack
      ) :-
  functor(Id, F, K), % make the cut green
  F/K \= array/2,   
  replaceVars(Stack, VExpr, VExpr1),
  Value is VExpr1,
  replaceOrPush(Stack, Id, Value, NewStack).



% replaceOrPush(+Assoc, +Id, +Value, -NewStack)
replaceOrPush([], Id, Value, [pair(Id, Value)]).
replaceOrPush([X | XS], Id, Value, [X1 | XS]) :- 
  X =.. [F, Id, _],
  !,
  X1 =.. [F, Id, Value].

replaceOrPush([X | XS], Id, Value, [X | YS]) :-
  replaceOrPush(XS, Id, Value, YS).



assocDelete([], _, []).

assocDelete([X | XS], K, YS) :-
  X =.. [_, K, _],
  !,
  assocDelete(XS, K, YS).

assocDelete([X | XS], K, [X | YS]) :-
  assocDelete(XS, K, YS).



% map(@List, :P/2, @NewList)
map([], _, []).
map([X|XS], P, [Y|YS]) :-
  call(P, X, Y),
  map(XS, P, YS).



dummyStackState(state(3, 10000, [], [], [])).
dummyProgram(
  program([sekcja])
).
