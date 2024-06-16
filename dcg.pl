% Zuzanna Surowiec, 438730
%
% Uproszczenia:
% 1. Reguły nie zawierają zmiennych postaci `V{liczba naturalna}`, np. V0, V1 itd. 

:- [library(lists)].
:- set_prolog_flag(double_quotes, codes).



% rt_file(+Path)
%
% Transforms 0 or more rules 
% from the specified file.
%
rt_file(Path) :-
  open(Path, read, S),
  read_file(S, Lines),
  append(Lines, Content),
  phrase(rt, Content).



% rt//0
%
% Transforms 0 or more DCG rules, 
% and prints them to the screen.
%
rt -->
  transform(R),
  !,
  { format("~s\n\n", [R]) },
  rt.

rt --> 
  [].


% transform(-Repr)
%
% Transforms a DCG rule (in text) to the text representation of its clause.
%
transform(R) -->
  { % initialise the first variables
    var_prefix(V),
    my_set_flag(V, 0),
    genvar(In)
  },
  token(fragment(In, Head, Out)),
  (   token(p_head(Out, Out, [], URetBody, X1)),
      { append(URetBody, RetBody) }
  ->  { 
        RetPrefix = [ X1, " = ", X2, ",\n  " ],
        RetSuffix = [ RetBody, "." ] 
      }
  ;   { 
        RetPrefix = [ X2, " = ", Out, ",\n  " ],
        RetSuffix = [ "." ] 
      }
  ),
  token("-->"),
  token(p(In, URuleBody, X2)),
  { % combine the rule body, and the return statements
    append(URuleBody, RuleBody),
    append([
      [ Head, " :- \n  " ],
      RetPrefix,
      [ RuleBody ], 
      RetSuffix
    ], ClauseParts),
    append(ClauseParts, R) 
  },
  { % cleanup     
    my_set_flag(V, 0)
  }.



% @spec p_head(+String, +String, +[String], -[String], -String)
% p_head(+InOriginal, +InCurrent, +U, -R, -Out) 
%
% Accepts the return instructions of a DCG rule head.
%
p_head(Io, Ic, U, R, In) -->
  token(","),
  !,
  fragment(Ic, X, Ic1),
  { U1 = [X, ",\n  " | U] },
  p_head(Io, Ic1, U1, R, In).

p_head(_, Ic, U, R, Ic), "-->" -->
  token("-->"),
  !,
  { reverse(U, R) }.



% @spec p(+String, -[String], ?String)
% p(+In, -Rep, ?Out)
%
% Accepts a list of instructions of a DCG rule.
%
p(In, R, Out) -->
  fragment(In, F, Ic),
  p(In, Ic, [F], R, Out).



% @spec p(+String, +String, +[String], -[String], ?String) 
% p(+InOriginal, +InCurrent, +Accumulator, -Repr, ?InNew)
%
% Accepts the rest of a list of instructions of a DCG rule.
% 
p(_, Ic, U, R, Ic) -->
  token("."),
  !,
  { reverse(U, R) }.



p(Io, Ic, U, R, In), ")" -->
  token(")"),
  !,
  { ground(In) },
  {
    (   Io = Ic
    ->  U1 = U
    ;   U1 = [ Io, " = ", Ic, ",\n  " | U ]
    ),
    U2 = [ Ic, " = ", In, ",\n  " | U1 ]
  },
  { reverse(U2, R) }.

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



% @spec fragment(+String, -String, -String)
% fragment(+In, -Repr, -Out)
%
% Accepts instructions of a DCG rule.
% `In` is input list.
% `Out` is the output list.
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
  { genvar(Out) },
  p(In, U, Out),
  token(")"),
  { 
    append(U, UR),
    append([ "(\n  ", UR, "\n)" ], R) 
  }.

fragment(In, "true", In) -->
  token("\"\""),
  !.

fragment(In, R, Out) -->
  token(str(Str)),
  !,
  { genvar(Out) },
  { append(["append(", Str, ", ", Out, ", ", In, ")"], R) }.

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



% nat(-N)
%
% Accepts a natural, decimal number.
% Allows for leading zeros.
%
nat(N) --> 
  my_many(digit, N).



% num(-N)
%
% Accepts a decimal integer.
% Allows for leading zeros, 
% as well as for optional 
% "-", and "+" prefixes.
%
num(N) -->
  (   "-" 
  ->  nat(N0), 
      { N = [0'- | N0] } 
  ;   optional("+"), 
      nat(N)
  ).



% str(-S)
%
% Accepts a string while 
% respecting escape characters.
%
str(S) -->
  spaces,
  "\"",
  str("\"", U),
  spaces,
  { reverse(U, S) }.


% str(+U, -S)
%
% A helper of str//1.
%
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



% lst(-R)
%
% Accepts a list of elements.
%
lst(R) -->
  token("["),
  lst_elems(Elems),
  token("]"),
  { append(["[", Elems, "]"], R) }.



% lst_elems(-Elems)
%
% Accepts the elements of a list.
% Does not check for its validity.
%
lst_elems(Elems) -->
  sep_by(
    trm,
    one_of([
      parse_rep(token(","), ","),
      parse_rep(token("|"), "|")
    ]),
    Elems
  ).



% predicate_call(-R)
%
% Accepts a predicate call or a functor.
%
predicate_call(R) -->
  predicate_name(N),
  args(Args),
  { append(N, Args, R) }.



% tuple(?R)
%
% Accepts a comma-separated list.
%
tuple(R) -->
  token("("),
  args(Xs),
  token(")"),
  { append(["(", Xs, ")"], R) }.



% args(?Xs)
%
% Accepts the elements of a list of arguments 
% (a comma-separated list of expressions without the parentheses).
%
args(Xs) -->
  sep_by(
    trm,
    parse_rep(token(","), ","),
    Xs
  ).



% trm(?X)
% 
% Accepts an expression term 
% (a fragment of an argument list or a list).
% 
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
%
% Accepts a predicate symbol or a functor name,
% returning it in Name.
%
predicate_name([X|XS]) -->
  lowercase(X),
  !,
  my_some(one_of([letter, digit, underscore]), XS).

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
  my_some(one_of([letter, digit, underscore]), XS).



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
%
letter(R) -->
  uppercase(R); lowercase(R).



% digit(?R)
%
% Accepts a single digit.
%
digit(R) -->
  [R],
  { R >= 0'0, R =< 0'9 }.


% one_of(+[:G//1], ?Y)
%
% Accepts one of the given predicates,
% and returns the result in Y.
one_of(Gs, Y) -->
  { member(G, Gs) },
  call(G, Y).




% my_some(:G//1, -U)
%
% Accepts 0 or more successes of G,
% and accumulates them in U.
%
my_some(G, [Y | YS]) -->
  call(G, Y),
  !,
  my_some(G, YS).

my_some(_, []) --> [].



% my_many(:G//1, ?U)
%
% Accepts 1 or more successes of G,
% and accumulates them in U.
%
my_many(G, [Y | YS]) -->
  call(G, Y),
  my_some(G, YS).



% optional(:G//0)
%
% Accepts either G or an empty string.
optional(G) --> G, !.
optional(_) --> [].



% inside(+L, +R, -Inside)
%
% Accepts the shortest substring between L and R.
%
inside(L, R, Inside) -->
  L,
  inside(L, R, [], Inside).


% inside(+L, +R, +Acc, -Inside)
%
% An inside//3 helper.
%
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



% token(:G//0)
%
% Trims spaces around G, and 
% accepts strings accepted by G.
%
token(G) --> spaces, G, spaces.



% spaces
%
% Ignores leading spaces.
%
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
%
genvar(S) :-
  var_prefix(V),
  gensym(V, S).



% parse_rep(:G//0, ?Rep, ?Rep)
%
% Accepts characters accepted by G,
% and returns Y. 
% Used as a way of representing 
% higher-order predicates, say:
% ```
% parse_rep(token(","), ",")
% ```
%
parse_rep(G, Y, Y) -->
  G.



% gensym(+Nazwa, -Symbol)
%
% Generates a string in the form of "{Id}{I}" 
% where "I" is the current value of the Id flag.
%
gensym(Id, Sym) :-
  nonvar(Id),
  my_get_flag(Id, OldVal),
  NewVal is OldVal + 1,
  my_set_flag(Id, NewVal),
  number_codes(OldVal, OldValStr),
  append(Id, OldValStr, Sym).



% The variable prefix.
var_prefix("V").



% read_file(+Stream, -Lines)
%
% Reads the stream lines into a list.
%
read_file(Stream, []) :-
    at_end_of_stream(Stream),
    !.

read_file(Stream, [X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, X0),
    append(X0, "\n", X),
    read_file(Stream, L).


  
% my_get_flag(+Name, -Value)
my_get_flag(Id, Val) :-
  ( gs(Id, Val) -> true
  ; Val = 0
  ).



% my_set_flag(+Name, +Value)
my_set_flag(Id, Val) :-
  retractall(gs(Id, _)),
  asserta(gs(Id, Val)).

