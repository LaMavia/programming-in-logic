num(N) -->
  (   "-" 
  ->  nat(N0), 
      { N = [0'- | N0] } 
  ;   optional("+"), 
      nat(N)
  ).

