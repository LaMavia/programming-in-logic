% mfunctor(?Term, ?Name, ?Arity)
mfunctor(Term, Name, Arity) :-
  var(Term), !,
  atom(Name),
  integer(Arity),
  Arity >= 0,
  length(Args, Arity),
  Term =.. [ Name | Args ].
  
mfunctor(Term, Name, Arity) :-
  nonvar(Term),
  Term =.. [ Name | Args ],
  length(Args, Arity).

% atom/1 - jest stałą
% atomic/1 - 

% marg(?N, +Term, ?Value)
marg(N, Term, Value) :-
  nonvar(Term),
  Term =.. [ _ | Args ],
  nth1(N, Args, Value).
