% showDerivation(+G)

:- expects_dialect(sicstus).
:- use_module(library(varnumbers)).

showDerivation(G) :-
  format("~nLD-dowod:~n", []),
  showDerivation(G, 0).

% zasada umieszczania odcięć: najwcześniej, 
% jak wiemy, że mamy właściwą klauzulę!!
showDerivation(true, N) :-
  !,
  format("G~d: true~n", [N]),
  format("Koniec LD-dowodu długości ~d.~n", [N]).

showDerivation(G, N) :-
  % ~q jak ~p, ale dodaje apostrofy, gdzie trzeba
  % G \= true,
  numbervars(G),
  format("G~d: <- ~q~n", [N, G]),
  varnumbers(G, CG),
  resolve(CG, G1),
  N1 is N + 1,
  showDerivation(G1, N1).



% 
resolve((H, T), G) :-
  !,
  % f(H, T, G)
  if( clause(H, B)
    , scal(B, T, G)
    , (format("   Goal failed, backtracking~n"), fail)
  ).
 

resolve(A, G) :-
  % f(A, true, G).
  if( clause(A, G)
    , true
    , (format("   Goal failed, backtracking~n"), fail) 
    ).



% mclause(A, G) :-
%   if( clause(A, B)
%     , ...
%     , (format("   Goal failed, backtracking~n"), fail) 
%     ).


% scal(+A, -B, ?R) - scalanie list przecinkowych z 
% pominięciem A=true.
scal(true, A, A) :- !.

scal((H, T), V, (H, T2)) :-
  !,
  scal(T, V, T2).
  
scal(A, B, (A, B)).








