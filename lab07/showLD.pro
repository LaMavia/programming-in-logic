% showLD(+G)

:- expects_dialect(sicstus).
:- use_module(library(varnumbers)).

showLD(G) :-
  format("~nLD-dowod:~n", []),
  showLD(G, 0).

% zasada umieszczania odcięć: najwcześniej, 
% jak wiemy, że mamy właściwą klauzulę!!
showLD(true, N) :-
  !,
  format("G~d: true~n", [N]),
  format("Koniec LD-dowodu długości ~d.~n", [N]).

showLD(G, N) :-
  % ~q jak ~p, ale dodaje apostrofy, gdzie trzeba
  % G \= true,
  numbervars(G),
  format("G~d: <- ~q~n", [N, G]),
  varnumbers(G, CG),
  findNextRes(CG, G1), % kolejna resolwenta w LD dowodzie
                       % clause, bez if-a
                       %
  N1 is N + 1,
  showLD(G1, N1).



% 
findNextRes((H, T), G) :-
  !,
  clause(H, B), 
  scal(B, T, G),
  % jak wywołać G bez ukonkretnienia?
  % Używamy negacji, gdyż to nie uzgadnia
  \+ \+ G.
 

findNextRes(A, G) :-
  clause(A, G), 
  \+ \+ G.



% mclause(A, G) :-
%   if( clause(A, B)
%     , ...
%     , 
%     ).


% scal(+A, -B, ?R) - scalanie list przecinkowych z 
% pominięciem A=true.
scal(true, A, A) :- !.

scal((H, T), V, (H, T2)) :-
  !,
  scal(T, V, T2).
  
scal(A, B, (A, B)).








