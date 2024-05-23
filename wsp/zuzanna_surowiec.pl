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
valSet(state(Is, PCs, Stack)
      , array(Id, IExpr)
      , VExpr
      , state(Is, PCs, [v(array(Id, Index), Value) | Stack])
      ) :-
  !,
  Index is IExpr,
  Value is VExpr.

valSet(state(Is, PCs, Stack)
      , Id
      , VExpr
      , state(Is, PCs, [v(Id, Value) | Stack])
      ) :-
  functor(Id, F, K), % make the cut green
  F/K \= array/2,   
  Value is VExpr.

