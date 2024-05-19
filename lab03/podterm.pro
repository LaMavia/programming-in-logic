% podterm(?P, +T)
podterm(T, T).
podterm(P, T) :-
  functor(T, _, N),
  podterm(N, P, T).

podterm(N, P, T) :-
  N > 0,
  arg(N, T, SubT),
  podterm(P, SubT).

podterm(N, P, T) :-
  N > 1,
  M is N - 1,
  podterm(M, P, T).


% podterm2(?P, +Term)
podterm2(T, T).
podterm2(P, T) :-
  T =.. [ _ | Args ],
  member(A, Args),
  podterm2(P, A).
