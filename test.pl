fragment(In, R, Out) -->
  spaces,
  predicate_name(Name),
  !,
  { genvar(Out) },
  ( inside(token("("), token(")"), Args)  
  ; g
  ).
  % (  inside(token("("), token(")"), Args),
  %    { Args \= [] }
  % -> { append([Name, "(", Args, ", ", In, ", ", Out, ")"], R) } 
  % ;  optional(empty_call),
  %    { append([Name, "(", In, ", ", Out, ")"], R) }
  % ).
  %
