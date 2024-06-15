% Zuzanna Surowiec 438730
:- op(700, xfx, <>).
:- [library(lists)].



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
  ;  verify(program(Instrs), N, S, CS, IS) 
  -> printUnsafe(CS, IS)
  ;  format("Program jest poprawny (bezpieczny).~n", [])
  ).

verify(_, File) :-
  format("Error: brak pliku o nazwie - ~s~n", [File]),
  fail.


% verify(+Program, +N, +State, -Steps, -InSection)
verify(Program, N, S0, CS, IS) :-
    allInterlacings(N, Program, S0, [], [], [], AllStates, AllStacks),
    nth0(Idx, AllStates, State), 
    nth0(Idx, AllStacks, CS), 
    isUnsafe(State, IS),
    !.

verify(_, _, _) :-
  !, 
  fail.

% allInterlacings(+N, +Program, +State, +CallStack, +StateList, +CallStackList,
% -NewStateList, -NewCallStackList)
allInterlacings(_, _, State, _, StateList, StackList, StateList, StackList) :-
    member(State, StateList).
allInterlacings(N, Program, State, Stack, StateList, StackList, NewStateList, NewStackList) :-
    \+ member(State, StateList),
    nextInstruction(N, Program, [State|StateList], [Stack|StackList], State, 0, NewStateList, NewStackList).

% nextInstruction(+N, +Program, +StateList, +CallStackList, +State, +Pid,
% -NewStateList, -NewCallStackList) 
nextInstruction(N, _, StateList, CallStackList, _, N, StateList,
    CallStackList).
nextInstruction(N, Program, StateList, StackList, State, Pid, NewStateList,
    NewStackList) :-
    Pid < N,
    NextPid is Pid + 1,
    nextInstruction(N, Program, StateList, StackList, State, NextPid, NSList, 
        NSStack),
    step(Program, State, Pid, NextState, InstrIdx),
    nth0(Idx, StateList, State),
    nth0(Idx, StackList, Stack),
    append(Stack, [pair(Pid, InstrIdx)], NextStack),
    allInterlacings(N, Program, NextState, NextStack, NSList, NSStack,
        NewStateList, NewStackList).



% printUnsafe(+Steps, +InSection)
printUnsafe(CS, IS) :-
  format("Program jest niepoprawny.~nNiepoprawny przeplot:~n", []),
  reverse(CS, CSR),
  printSteps(CSR),
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
%                 ^ `Id` or an array expression `array(Id, Index)`
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
%   S1 = state( 2, [pos(1, 1)]
%             , [pair(x, 0), pair(array(arr, 0), 0), pair(array(arr, 1), 0)]
%             ), 
%     H1 = []
%   S2 = state(2, [pos(1, 2)], [pair(x, 1), ...]) 
%     H2 = [S1]
%   S3 = state( 2, [pos(1, 1)]
%             , [..., pair(array(arr, 1), 5), pair(x, 1)]
%             ) 
%     H3 = [S2, S1]
% 
% Where H* is a list of previous states.
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
  (  X = V 
  -> LS = [N | LS1]
  ;  LS = LS1
  ),
  findIncides(XS, V, N1, LS1).



% step(+Program, +StanWe,
%     , ?PrId  , -StanWy, -InstrNr
%     )

step(P, S, Pid, S1, I) :-
  var(Pid),
  !,
  arg(2, S, N),
  step_gen(0, N, Pid, [P, S], [S1, I]).

step( program(Instrs)
    , State
    , Pid
    , state(SIx, N, L, NewPCs, NewStack)
    , PidPC
    ) :-
  State = state(SIx, N, L, PCs, Stack),
  integer(Pid),
  Pid >= 0,
  Pid < N,
  assocLookup(PCs, Pid, 1, PidPC),            % get the current pid PC
  nth0(PidPC, Instrs, Instr),                 % get the current instruction
  evalInstr( Instr                            % eval the instruction 
           , Pid
           , state(SIx, N, L, PCs, Stack)
           , state(SIx, N, L, NewPCs, NewStack)
           ).    
  

step_gen(Pid, N, _, _, _, _) :-
  (Pid < 0 ; Pid >= N),
  !,
  fail.

step_gen(Pid, N, Pid, LArgs, RArgs) :-
  Pid >= 0,
  Pid < N,
  append(LArgs, [Pid | RArgs], Args),
  G =.. [step | Args],
  format('~q', [G]),
  call(G).

step_gen(Pid0, N, Pid, LArgs, RArgs) :-
  Pid0 >= 0,
  Pid0 < N,
  Pid1 is Pid0 + 1,
  step_gen(Pid1, N, Pid, LArgs, RArgs).



% isUnsafe(+State, -PidsInSection)
isUnsafe(state(SIx, N, _, PCs, _), IS) :-
  isUnsafe(SIx, 0, N, PCs, IS),
  length(IS, L),
  L > 1.



% isUnsafe( +SectionLines
%         , +Pid
%         , +N
%         , +InstructionCounters
%         , -PidsInSection
%         )
isUnsafe(_, Pid, N, _, []) :-
  (Pid < 0 ; Pid >= N),
  !.

isUnsafe(SIx, Pid, N, PCs, IS) :-
  Pid < N,
  assocLookup(PCs, Pid, 1, PidPC),
  (  member(PidPC, SIx) 
  -> IS = [Pid | IS1]
  ;  IS = IS1
  ),
  Pid1 is Pid + 1,
  isUnsafe(SIx, Pid1, N, PCs, IS1).



% evalInstr(Instr, Stack, NewStack)
evalInstr( assign(Id, VExpr)
         , Pid
         , state(SIx, N, L, PCs, Stack)
         , state(SIx, N, L, PCs1, NewStack)
         ) :-
  valSet(Stack, Pid, Id, VExpr, NewStack),
  assocLookup(PCs, Pid, 1, PidPC),
  PidPC1 is PidPC + 1,
  replaceOrPush(PCs, Pid, PidPC1, PCs1).

evalInstr( goto(M)
         , Pid
         , state(SIx, N, L, PCs , Stack)
         , state(SIx, N, L, PCs1, Stack)
         ) :-
  replaceOrPush(PCs, Pid, M, PCs1).

evalInstr( condGoto(Cond, M)
         , Pid
         , State
         , NewState
         ) :-
  arg(5, State, Stack),
  replaceVars(Stack, Pid, Cond, Cond1),
  (  call(Cond1) 
  -> evalInstr(goto(M), Pid, State, NewState)
  ;  NewState = State
  ).

evalInstr(sekcja, _, State, State).



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



% replaceVars(+Stack, +Pid, +Expr, -Expr)
replaceVars(S, Pid, array(ArrName, IExpr), array(ArrName, IExpr1)) :-
  !,
  replaceVars(S, Pid, IExpr, IExpr1).

replaceVars(S, Pid, Expr, Expr1) :-
  Expr =.. [F, AExpr, BExpr],
  !,
  replaceVars(S, Pid, AExpr, AExpr1),
  replaceVars(S, Pid, BExpr, BExpr1),
  Expr1 =.. [F, AExpr1, BExpr1].

replaceVars(_, _, Expr, Expr) :-
  number(Expr),
  !.

replaceVars(_, Pid, pid, Pid) :- !.

replaceVars(S, _, VarName, VarValue) :-
  nonvar(VarName),
  functor(VarName, _, 0),
  VarName \= pid,
  valLookup(S, VarName, VarValue).



% valLookup(+Stack, +Id, -Value)
valLookup(Stack, array(Id, IExpr), Value) :-
  !,
  Index is IExpr,
  assocLookup(Stack, array(Id, Index), 0, Value).

valLookup(Stack, Id, Value) :-
  functor(Id, F, K),               % make the last cut green 
  F/K \= array/2,
  assocLookup(Stack, Id, 0, Value).



% assocLookup(+Assoc, +Key, +DefaultValue, -Value)
assocLookup([], _, Value, Value).
assocLookup([X | _], Key, _, Value) :-
  X =.. [_, Key, Value],
  !.
assocLookup([X | XS], Key, DefaultValue, Value) :-
  X =.. [_, OtherKey, _],                    % make the previous cut green
  OtherKey \= Key,
  assocLookup(XS, Key, DefaultValue, Value).



% valSet(+Stack, +Pid, +Id, +Value, -NewStack)
valSet( Stack
      , Pid
      , array(Id, IExpr)
      , VExpr
      , NewStack
      ) :-
  !,
  replaceVars(Stack, Pid, IExpr, IExpr1),
  replaceVars(Stack, Pid, VExpr, VExpr1),
  Index is IExpr1,
  Value is VExpr1,
  replaceOrPush(Stack, array(Id, Index), Value, NewStack).

valSet( Stack
      , Pid
      , Id
      , VExpr
      , NewStack
      ) :-
  functor(Id, F, K),                        % make the cut green
  F/K \= array/2,   
  replaceVars(Stack, Pid, VExpr, VExpr1),
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



% assocDelete(+Assoc, @Key, -Assoc)
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


dummyProgram(program([sekcja])).
