p(_, Ic, U, R, Ic) -->
  token("."),
  !,
  { reverse(U, R) }.



p(_, Ic, U, R, In), ")" -->
  token(")"),
  !,
  { ground(In) },
  {
    U1 = [ Ic, " = ", In, ",\n  " | U ],
    reverse(U1, R) 
  }.

p(Io, Ic, U, R, In) -->
  token(","),
  !,
  fragment(Ic, X, Ic1),
  { U1 = [X, ",\n  " | U]
  },
  p(Io, Ic1, U1, R, In).


