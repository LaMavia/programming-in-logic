% queens(+N, ?B)
% jak reprezeontować wynik: lista par - pozycje
% ale można lepiej: lista numerów wierszy, albo kolumn.
% Rozwiązanie: permutacja {1, ..., n}, więc możemy sprawdzać.
% Najgorsze, bo generujemy każdego, całego kandydata.
queens(N, B) :- 
    gen(N, L),  % generuje {1, ... , N} w dowolnej kolejności
    perm(L, B), % permutuje L, zapisuje do B
    safe(B).    % sprawdź

% gen(+N, -L)
gen(0, []). % :- !. odcięcie, nie ustawia punktu nawrotu - green cut
            % red cut - odcięcie jakieś odpowiedzi
gen(N, [N | L]) :-
    N > 0,
    M is N - 1,
    gen(M, L).


% perm(+L, ?P)
perm([], []).
perm([H | T], P) :-
    perm(T, PT),
    insert(PT, H, P).


% insert(+L, +H, -LH)
insert(L, H, [H | L]).
insert([LH | LT], H, [LH | L1]) :- 
    insert(LT, H, L1).


% safe(+B)
% sprawdzamy tylko przekątne, bo nasza reprezentacja resztę wyklucza
safe([]).
safe([ Q | Qs ]) :-
    \+ attacks(Q, Qs), % negacja poprawna tylko gdy w atomie tylko zmienne ustalone.
    safe(Qs).


% attacks(+Q, +Qs)
attacks(Q, Qs) :- attacks(Q, Qs, 1).

attacks(Q, [Qq | _], N) :- 
    Qq is Q + N;
    Qq is Q - N.

attacks(Q, [_ | Qqs], N) :- 
    N1 is N + 1,
    attacks(Q, Qqs, N1).
