transform(R) -->
  {
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
  { 
    append(URuleBody, RuleBody),
    append([
      [ Head, " :- \n  " ],
      RetPrefix,
      [ RuleBody ], 
      RetSuffix
    ], ClauseParts),
    append(ClauseParts, R) 
  },
  {    
    my_set_flag(V, 0)
  }.

