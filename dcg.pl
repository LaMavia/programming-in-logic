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

var_prefix("V").

rt_file(Path) :-
  open(Path, read, S),
  read_file(S, Lines),
  append(Lines, Content),
  phrase(rt, Content).



read_file(Stream, []) :-
    at_end_of_stream(Stream),
    !.

read_file(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X0),
    append(X0, "\n", X),
    read_file(Stream, L).

rt -->
  transform(R),
  !,
  { format("~s\n\n", [R]) },
  rt.

rt --> 
  [].


transform(R) -->
  { % initialise the first variables
    var_prefix(V),
    set_flag(V, 0),
    genvar(In)
  },
  token(fragment(In, Head, Out)),
  (  token(p_head(Out, Out, [], RetBody, X1))
  -> { RetSuffix = [ ",\n  ", X2, " = ", Out, ",\n  ", X1, " = ", X2, ",\n  ", RetBody, "." ] }
  ;  { RetSuffix = [ ",\n  ", X2, " = ", Out, "." ] }
  ),
  token("-->"),
  token(p(In, URuleBody, X2)),
  { % combine the rule body, and the return statements
    append(URuleBody, RuleBody),
    append([ Head, " :- \n  ", RuleBody ], RetSuffix, ClauseParts),
    append(ClauseParts, R) 
  },
  { % cleanup     
    set_flag(V, 0)
  }.



p_head(Io, Ic, U, R, In) -->
  token(","),
  !,
  fragment(Ic, X, Ic1),
  { U1 = [X, ",\n  " | U] },
  p_head(Io, Ic1, U1, R, In).



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
  { U1 = [X, ",\n  " | U]
  },
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
  { reverse(U, R) }.

p(_, Ic, U, R, Ic), ")" -->
  token(")"),
  !,
  { reverse(U, R) }.

p(_, Ic, U, R, Ic), "-->" -->
  token("-->"),
  !,
  { reverse(U, R) }.


% @spec fragment :: String -> String -> String
% fragment(+In, -Repr, -Out)
%
fragment(In, "!", In) -->
  token("!"),
  !.

fragment(In, "true", In) -->
  token("["),
  token("]"),
  !.

fragment(In, R, Out) -->
  token("["),
  !,
  lst_elems(Elems),
  { Elems \= [] },
  token("]"),
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
  { append(["phrase(\"", Elems, "\", ", In, ", ", Out, ")"], R) }.

fragment(In, R, In) -->
  token(inside(token("{"), token("}"), R)),
  !.

fragment(In, R, Out) -->
  var_name(V),
  !,
  { genvar(Out) },
  { append(["phrase(", V, ", ", In, ", ", Out, ")"], R) }.

fragment(In, R, Out) -->
  spaces,
  predicate_name(Name),
  !,
  { genvar(Out) },
  (   token("("),
      args(Args),
      token(")"),
      { Args \= [] }
  ->  { append([Name, "(", Args, ", ", In, ", ", Out, ")"], R) }
  ;   optional(empty_call),
      { append([Name, "(", In, ", ", Out, ")"], R) }
  ).



nat(N) --> 
  many(digit, N).



num(N) -->
  (  "-" 
  -> nat(N0), 
     { N = [0'- | N0] } 
  ;  optional("+"), 
     nat(N)
  ).



str(S) -->
  spaces,
  "\"",
  str("\"", U),
  spaces,
  { reverse(U, S) }.


str(U, S) -->
  "\\",
  !,
  [C],
  { U1 = [C, 0'\\ | U] },
  str(U1, S).

str(S, [0'" | S]) -->
  "\"",
  !.

str(U, S) -->
  [C],
  { \+ member(C, "\\\"") },
  { U1 = [C | U] },
  str(U1, S).



lst(R) -->
  token("["),
  lst_elems(Elems),
  token("]"),
  { append(["[", Elems, "]"], R) }.


lst_elems(Elems) -->
  sep_by(
    trm,
    one_of([
      parse_rep(token(","), ","),
      parse_rep(token("|"), "|")
    ]),
    Elems
  ).


predicate_call(R) -->
  predicate_name(N),
  args(Args),
  { append(N, Args, R) }.


tuple(R) -->
  token("("),
  args(Xs),
  token(")"),
  { append(["(", Xs, ")"], R) }.



args(Xs) -->
  sep_by(
    trm,
    parse_rep(token(","), ","),
    Xs
  ).

trm(X) --> 
  one_of([
    num, 
    str, 
    predicate_call, 
    predicate_name, 
    lst, 
    tuple, 
    var_name
  ], X).


% predicate_name(?Name)
predicate_name([X|XS]) -->
  lowercase(X),
  !,
  some(one_of([letter, digit, underscore]), XS).

predicate_name(R) -->
  inside("'", "'", X),
  { append(["'", X, "'"], R) }.
  
  
% empty_call//0
%
% Accepts an empty call ::= '(' ')'.
%
empty_call -->
  token("("),
  token(")").



% var_name(?Name)
%
% Accepts variables in the form of:
% V ::= ('_' + Uppercase) (Letter + Digit + '_')*
var_name([X|XS]) -->
  one_of([ uppercase, underscore ], X),
  some(one_of([letter, digit, underscore]), XS).



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



many(G, [Y | YS]) -->
  call(G, Y),
  some(G, YS).



% optional(:G//0)
optional(G) --> G, !.
optional(_) --> [].


% inside(+L, +R, -Inside)
inside(L, R, Inside) -->
  L,
  inside(L, R, [], Inside).

% inside(+L, +R, +Acc, -Inside)
inside(_, R, U, Inside) -->
  R,
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



% sep_by(:E//1, :S//1, ?Seq)
%
% Accepts a sequence of elements accepted by E,
% separated by elements accepted by S.
% Returns a string representing the sequence. 
%
sep_by(E, S, YS) -->
  call(E, Y),
  !,
  sep_by(E, S, [Y], YS).

sep_by(_, _, []) --> [].

sep_by(E, S, U, YS) -->
  token(call(S, Sep)),
  !,
  call(E, Y),
  { U1 = [Y, Sep | U] },
  sep_by(E, S, U1, YS).

sep_by(_, _, U, YS) -->
  {
    reverse(U, UR),
    append(UR, YS)
  }.


% genvar(-Symbol)
%
% Generates a string representing a fresh
% stream variable starting with "$S".
genvar(S) :-
  var_prefix(V),
  gensym(V, S).



parse_rep(G, Y, Y) -->
  G.



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
  % append([Id, "(", OldValStr, ")"], Sym).

