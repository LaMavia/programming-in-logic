% queens(+N, -B)
smqueens(N, B) :-
    gen(N, L), % L - zbiór queens do ustalenia
    insert(L, [], B).

% rozszerzamy rozwiązania częściowe
sminsert([], B, B).
sminsert(L, P, B) :-
    select(H, L, LH), % H in L, L ~= H + LH
    \+ attacks(H, P),
    insert(LH, [H | P], B).

% attacks(+Q, +Qs)
smattacks(Q, Qs) :- attacks(Q, Qs, 1).

smattacks(Q, [Qq | _], N) :- 
    Qq is Q + N;
    Qq is Q - N.

smattacks(Q, [_ | Qqs], N) :- 
    N1 is N + 1,
    attacks(Q, Qqs, N1).
