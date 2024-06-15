:- [library(lists)].
:- set_prolog_flag(double_quotes, codes).

% $i: [X] -> $i = [X | $(i+1)]     : $(i+1)
% $i: X   -> phrase(X, $i, $(i+1)) : $(i+1)
% $i: {X} -> X                     : $i

% T(p(X), W1, ..., Wn --> B1, ..., Bn)
%  = p(X, $0, $n) :-

% flags:
% iota

% p(X), B1, B2, B3 --> A1, A2, A3.

% p -->
%   ( A -> "a", "b"
%   ; B -> "c", "d"
%   ; [C, D, E]
%   ).
%

transform((Head :- Body)) -->
  { % initialise the first variables
    set_flag(iota, 0),
    gensym("$S", In),
    gensym("$S", Out)
  },
  token(fragment(In, Head, Out)),
  token(transform_seq_rest(Out, X, RetBody)),
  token("-->"),
  token(transform_seq(In, X, RuleBody)),
  {     
    set_flag(iota, 0) 
  }.


% @spec p :: String
% p(-Repr)
%
% p(R) -->
%   { 
%     genvar(In)
%   },
%   token(fragment(In, Head, Out)),
%


% @spec p(+String, -[String], ?String)
% p(+In, -Rep, ?Out)
%
p(In, R, Out) -->
  fragment(In, F, Ic),
  p(In, Ic, [F], R, Out).


% @spec p(+String, +String, +[String], -[String], -String) 
% p(+InOriginal, +InCurrent, +Accumulator, -Repr, -InNew)
% 
p(Io, Ic, U, R, In) -->
  token(","),
  !,
  fragment(Ic, X, Ic1),
  { U1 = [X, ",\n  " | U] },
  p(Io, Ic1, U1, R, In).

p(Io, Ic, U, R, In) -->
  token(";"),
  !,
  fragment(Io, X, Ic1),
  { U1 = [X, "\n; ", US, ",\n  " | U] },
  p(Io, Ic1, U1, R, In),
  { % Unify the branch outputs.
    % Calling it before p//5 leads 
    % to excessive backtracking.
    append([Ic, " = ", In], US)
  }.

p(Io, Ic, U, R, In) -->
  token("->"),
  !,
  fragment(Ic, X, Ic1),
  { U1 = [X, "\n->" | U] },
  p(Io, Ic1, U1, R, In).

p(_, Ic, U, R, Ic) -->
  token("."),
  !,
  { 
    U1 = ["." | U],
    reverse(U1, R)
  }.

p(_, Ic, U, R, Ic), ")" -->
  token(")"),
  !,
  { reverse(U, R) }.


% @spec fragment :: String -> String -> String
% fragment(+In, -Repr, -Out)
%
fragment(In, "true", In) -->
  token("["),
  token("]"),
  !.

fragment(In, R, Out) -->
  token(inside("[", "]", Elems)),
  !,
  { genvar(Out) },
  { append([In, " = ", "[", Elems, " | ", Out, "]"], R) }.

fragment(In, R, Out) -->
  token("("),
  !,
  p(In, U, Out),
  token(")"),
  { 
    append(U, UR),
    append([ "( ", UR, "\n)" ], R) 
  }.

fragment(In, "true", In) -->
  token("\"\""),
  !.

fragment(In, R, Out) -->
  token(inside("\"", "\"", Elems)),
  !,
  { genvar(Out) },
  { append(["phrase(\"", Elems, "\", ", Out, ", ", In, ")"], R) }.

fragment(In, R, In) -->
  token(inside("{", "}", R)),
  !.

fragment(In, R, Out) -->
  var_name(V),
  !,
  { genvar(Out) },
  { append(["phrase(", V, ", ", In, ", ", Out, ")"], R) }.



% var_name(?Name)
%
% Accepts variables in the form of:
% V ::= ('_' + Uppercase) (Letter + Digit + '_')*
var_name([X|XS]) -->
  one_of([ uppercase, underscore ], X),
  some(one_of([letter, digit]), XS).



% underscore(?Y)
% 
% Accepts a single underscore.
%
underscore(C) --> 
  [C],
  { C = 0'_ }.



% uppercase(?R)
% 
% Accepts a single uppercase character.
%
uppercase(R) -->
  [R],
  { R >= 0'A, R =< 0'Z }.



% lowercase(?R)
%
% Accepts a single lowercase character.
%
lowercase(R) -->
  [R],
  { R >= 0'a, R =< 0'z }.



% letter(?R)
% 
% Accepts one letter.
letter(R) -->
  uppercase(R); lowercase(R).



% digit(?R)
%
% Accepts a single digit.
digit(R) -->
  [R],
  { R >= 0'0, R =< 0'9 }.


% one_of(+[:G//1], ?Y)
one_of(Gs, Y) -->
  { member(G, Gs) },
  call(G, Y),
  !.




% some(:G//1, -U)
%
% Accepts 0 or more successes of G,
% and accumulates them in U.
some(G, [Y | YS]) -->
  call(G, Y),
  !,
  some(G, YS).

some(_, []) --> [].


% inside(+L, +R, -Inside)
inside(L, R, Inside) -->
  L,
  inside(L, R, [], Inside).

% inside(+L, +R, +Acc, -Inside)
inside(_, R, U, Inside) -->
  R,
  !,
  {reverse(U, Inside)}.

inside(L, R, U, Inside) -->
  [C],
  { % make the cut green
    C \= R 
  },
  { % add the new char to the accumulator
    U1 = [C | U] 
  },
  inside(L, R, U1, Inside).


% fragment(-In, -Out, -Parsed) 
% fragment(In, Out, (In = [Elems | Out])) -->



% token(:G//0)
token(G) --> spaces, G, spaces.



% spaces
spaces --> [C], { member(C, [9, 10, 11, 12, 13, 32]) }, !, spaces.
spaces --> [].



% genvar(-Symbol)
%
% Generates a string representing a fresh
% stream variable starting with "$S".
genvar(S) :-
  gensym("'$VAR'", S).



% gensym(+Nazwa, -Symbol)
%
% Generates a string in the form of "IdI" 
% where "I" is the current value of the Id flag.
gensym(Id, Sym) :-
  nonvar(Id),
  get_flag(Id, OldVal),
  NewVal is OldVal + 1,
  set_flag(Id, NewVal),
  atom_codes(OldVal, OldValStr),
  append([Id, "(", OldValStr, ")"], Sym).
