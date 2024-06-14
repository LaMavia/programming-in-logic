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
  token(fragment(In, Out, Head)),
  token(transform_seq_rest(Out, X, RetBody)),
  token("-->"),
  token(transform_seq(In, X, RuleBody)),
  {     
    set_flag(iota, 0) 
  }.


% @spec p :: String -> String -> [String] -> [String] -> String
% p(+InOriginal, +InCurrent, +Accumulator, -Repr, -InNew)
% 
p(Io, Ic, U, R, In) -->
  token(","),
  !,
  fragment(Ic, X, Ic1),
  U1 = [X, "," | U],
  p(Io, Ic1, U1, R, In).


p(Io, In, U, R, In) -->
  token(";"),
  !,
  fragment(Io, X, Ic),
  U1 = [X, ";" | U],
  p(Io, Ic, U1, R, In).


p(Io, Ic, U, R, In) -->
  token("->"),
  !,
  fragment(Ic, X, Ic1),
  U1 = [X, "->" | U],
  p(Io, Ic1, U1, R, In).


p(_, Ic, U, R, Ic) -->
  token("."),
  !,
  reverse(U, R).

% @spec fragment :: String -> String -> String
% fragment(+In, -Repr, -Out)
%
fragment(In, "true", In) -->
  token("[]"),
  !.

fragment(In, R, Out) -->
  token(inside("[", "]", Elems)),
  { genvar(Out) },
  !,
  { append([In, " = ", "[", Elems, " | ", Out, "]"], R) }.





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

% sep(?Repr)
% Consumes skippable fragments, and returns their trimmed representation.
% Succeeds at most once, so fragments need be ordered correctly.
sep(T) --> 
  { member(T, [",", ".", ";", "->", "(", ")"]) },
  token(T),
  !.


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
  gensym("$S", S).



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
  append(Id, OldValStr, Sym).
