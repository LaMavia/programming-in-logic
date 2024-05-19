% testQueens(+NazwaPredykatu, +N, +Test)
%
% ```pro
% testQueens(queens1, 8, runtime),
% testQueens(queens3, 12, total_runtime).
% ```

wszystkie(G) :-
  call(G), % albo G 
  fail.
wszystkie(_). % żeby się zwróciło


% statistics/2
%

testQueens(P, N, Test) :- 
  functor(Atom, P, 2),
  arg(1, Atom, N),
  statistics(Test, [ T1 | _ ]),
  wszystkie(Atom),
  statistics(Test, [ T2 | _ ]),
  T is T2 - T1,
  format('took ~3d sec.~n', [T]).


