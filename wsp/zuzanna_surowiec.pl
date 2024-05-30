:- op(700, xfx, <>).
:- [library(lists)].

% state representation:
% state(InSection, LenInstrs, PCs, Stack, StateHistory)
%   InSection: number of processes in section
%   LenInstrs: number of instructions
%   PCs: [pos(Pid, InstructionNumber)]
%   Stack: [v(Key, Value)]
%             ^ `Id` or an array expression `array(Id, Index)`
%   StateHistory: [S0, ..., S(k-1)]
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
%   S1 = state(2, [pos(1, 1)], [], [])
%   S2 = state(2, [pos(1, 2)], [v(x, 1)], [S1])
%   S3 = state(2, [pos(1, 1)], [v(array(arr, 1), 5), v(x, 1)], [S2, S1])
% 
% initState(+Program, +N, -StanPoczątkowy)
initState(program(Instrs), _, state(0, L, [], [], [])) :-
  length(Instrs, L).

% before step:
% unsafe - succeed
% safe, already seen - fail
% safe - go on


% step(+Program, +StanWe, ?PrId, -StanWy)
step(_, State, _, History) :-
  member(State, History),
  !,
  fail.

step( program(Instrs)
    , state(InSection, LenInstrs, PCs, Stack, History),
    , Pid
    , state(NewPCs, NewStack, [State|History])
  ) :-
  assocLookup(PCs, Pid, 1, PidPC),               % get the current pid PC
  valSet(Stack, pid, Pid, EvalStack),        % set the pid var
  nth1(PidPC, Instrs, Instr),                % get the current instruction
  evalInstr(Instr, Stack, NewStack),         % eval the instruction 
  NewPidPC is (PidPC - 1) mod LenInstrs + 1,     % move to the next instruction
  replaceOrPush(PCs, Pid, NewPidPC, NewPCs).     % update the pid PC 
  



% evalInstr(Instr, Stack, NewStack)
% evalInstr



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
  !,
  replaceVars(S, IExpr, IExpr1).

replaceVars(S, Expr, Expr1) :-
  Expr =.. [F, AExpr, BExpr],
  !,
  replaceVars(S, AExpr, AExpr1),
  replaceVars(S, BExpr, BExpr1),
  Expr1 =.. [F, AExpr1, BExpr1].

replaceVars(_, Expr, Expr) :-
  number(Expr),
  !.

replaceVars(S, VarName, VarValue) :-
  functor(VarName, _, 0),
  valLookup(S, VarName, VarValue).



% valLookup(+Stack, +Id, -Value)
valLookup(Stack, array(Id, IExpr), Value) :-
  !,
  Index is IExpr,
  assocLookup(Stack, array(Id, Index), 0, Value).

valLookup(state(_, Stack, _), Id, Value) :-
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
replaceOrPush([], Id, Value, [v(Id, Value)]).
replaceOrPush([X | XS], Id, Value, [X1 | XS]) :- 
  X =.. [F, Id, _],
  !,
  X1 =.. [F, Id, Value].

replaceOrPush([X | XS], Id, Value, [X | YS]) :-
  replaceOrPush(XS, Id, Value, YS).



% map(@List, +P/2, @NewList)
map([], _, []).
map([X|XS], P, [Y|YS]) :-
  G =.. [P, X, Y],
  call(G),
  map(XS, P, YS).



dummyStackState(state([], [], [])).
