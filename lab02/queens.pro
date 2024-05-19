% typowe rozwiązanie: uzgadnianie
% N elementowa liczba ze zmiennymi (można `length` użyć)
pqueens(N, B) :- 
    length(B, N),
    place_queens(N, B, _, _). % _ to przekątne górna i dolna dla hetmanów.

place_queens(0, _, _, _).
place_queens(K, B, [ H | T ], D) :- % niezmiennik: 
    K > 0, % ważne!!!
    J is K - 1,
    % przekątne skracamy/rozszerzamy odpowiednio.
    % placeujemy pozcjonując wg. odległości od początku wiersza.
    place_queens(J, B, T, [ _ | D ]), % taka kolejność (bez ogona), bo 
                                      % nie zawsze najważniejsza ogonowość, a
                                      % dostajemy mniejszy niedeterminizm 
                                      % - dużo mniej nawrotów do `place_queen`.
                                      %
    place_queen(K, B, [ H | T ], D).

% tu uzgadniamy.
place_queen(K, [ K | _ ], [ K | _ ], [ K | _ ]).
place_queen(K, [ _ | B ], [ _ | U ], [ _ | D ]) :- 
    place_queen(K, B, U, D).


