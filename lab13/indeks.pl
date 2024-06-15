:- set_prolog_flag(double_quotes, codes).

% indeks(+P, +I0, -I).
indeks(W, T, K) :-
  indeks(W, 1, K, T, _). % <- out
%                 ^ in

indeks(W, I, I) --> W.
indeks(W, I, K) --> [_], {I1 is I + 1}, indeks(W, I1, K).


liczba(K) --> [C], {cyfra(C, W)}, liczba(W, K).
liczba(W, K) --> [C], {cyfra(C, WC)}, !, { Z is 10 * W + WC }, liczba(Z, K).
liczba(K, K) --> [].

% f(P) --> P.

% indeks(P, T, I) :-
%   indeks(P, T, 0, I).
