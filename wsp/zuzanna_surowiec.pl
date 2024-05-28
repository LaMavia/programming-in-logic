:- op(700, xfx, <>).

% state representation:
% state(Instructions, PCs, Stack)
%   Instructions: [instr(Index, Instruction)] 
%   PCs: [pos(Pid, InstructionNumber)]
%   Stack: [v(Key, Value)]
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
%   1. state([instr(1, assign(x, 1)), instr(2, assign(array(arr, 1), 5))], [pos(1, 1)], [])
%   2. state([instr(1, assign(x, 1)), instr(2, assign(array(arr, 1), 5))], [pos(1, 2)], [v(x, 1)])
%   3. state([instr(1, assign(x, 1)), instr(2, assign(array(arr, 1), 5))], [pos(1, 1)], [v(array(arr, 1), 5), v(x, 1)])
% 
% initState(+Program, +N, -StanPoczątkowy)



% step(+Program, +StanWe, ?PrId, -StanWy)



% replaceOperators(+Expr, -Expr)
replaceOperators((AExpr <> BExpr), (AExpr1 =\= BExpr1)) :-
  !,
  replaceOperators(AExpr, AExpr1),
  replaceOperators(BExpr, BExpr1).

replaceOperators((AExpr = BExpr), (AExpr1 =:= BExpr1)) :-
  !,
  replaceOperators(AExpr, AExpr1),
  replaceOperators(BExpr, BExpr1).

replaceOperators(Expr, Expr1) :-
  Expr =.. [F, AExpr, BExpr],
  !,
  replaceOperators(AExpr, AExpr1),
  replaceOperators(BExpr, BExpr1),
  Expr1 =.. [F, AExpr1, BExpr1].

replaceOperators(Expr, Expr).


% replaceVars(+State, +Expr, -Expr)
replaceVars(S, Expr, Expr1) :-
  Expr =.. [F, AExpr, BExpr],
  member(F, [+, -, *, /, =:=, =/=, <]),
  !,
  replaceVars(S, AExpr, AExpr1),
  replaceVars(S, BExpr, BExpr1),
  Expr1 =.. [F, AExpr1, BExpr1].

replaceVars(S, array(ArrName, IExpr), array(ArrName, IExpr1)) :-
  !,
  replaceVars(S, IExpr, IExpr1).

replaceVars(_, Expr, Expr) :-
  number(Expr),
  !.

replaceVars(S, VarName, VarValue) :-
  functor(VarName, _, 0),
  valLookup(S, VarName, VarValue).



% valLookup(+State, +Id, -Value)
valLookup(state(_, _, Stack), array(Id, IExpr), Value) :-
  !,
  Index is IExpr,
  auxValLookup(Stack, array(Id, Index), Value).

valLookup(state(_, _, Stack), Id, Value) :-
  functor(Id, F, K),              % make the last cut green 
  F/K \= array/2,
  auxValLookup(Stack, Id, Value).

% auxValLookup(+Stack, +Id, -Value)
auxValLookup([], _, 0) :- !.
auxValLookup([v(Key, Value) | _], Key, Value) :- !.
auxValLookup([v(OtherKey, _) | ES], Key, Value) :- 
  OtherKey \= Key,              % make the last cut green
  auxValLookup(ES, Key, Value).



% valSet(+State, +Id, +Value, -NewState)
valSet( State
      , array(Id, IExpr)
      , VExpr
      , state(Is, PCs, NewStack)
      ) :-
  State = state(Is, PCs, Stack),
  !,
  replaceVars(State, IExpr, IExpr1),
  replaceVars(State, VExpr, VExpr1),
  Index is IExpr1,
  Value is VExpr1,
  replaceOrPush(Stack, array(Id, Index), Value, NewStack).

valSet( State
      , Id
      , VExpr
      , state(Is, PCs, NewStack)
      ) :-
  State = state(Is, PCs, Stack),
  functor(Id, F, K), % make the cut green
  F/K \= array/2,   
  replaceVars(State, VExpr, VExpr1),
  Value is VExpr1,
  replaceOrPush(Stack, Id, Value, NewStack).



% replaceOrPush(+Stack, +Id, +Value, -NewStack)
replaceOrPush([], Id, Value, [v(Id, Value)]).
replaceOrPush([v(Id, _) | T], Id, Value, [v(Id, Value) | T]) :- !.
replaceOrPush([_ | Stack], Id, Value, NewStack) :-
  replaceOrPush(Stack, Id, Value, NewStack).


dummyStackState(state([], [], [])).
