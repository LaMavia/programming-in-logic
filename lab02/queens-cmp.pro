% Mierzymy, ile zajęły nam wszystki

% wszystkie(+G)
% G - goal
%
% explicite fail-driven loop
%

% Bierzemy każdy element Z1, sprawdzamy, czy najeży do Z2.
intersect(Z1, Z2) :- member(X, Z1), member(X, Z2).

wszystkie(G) :-
  call(G), % albo G 
  fail.
wszystkie(_). % żeby się zwróciło


% statistics/2
%

test(G) :- 
  statistics(runtime, [ T1 | _ ]),
  wszystkie(G),
  statistics(runtime, [ T2 | _ ]),
  T is T2 - T1,
  format('took ~3d sec.~n', [T]).

