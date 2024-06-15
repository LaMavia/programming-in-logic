:- set_prolog_flag(double_quotes, codes).

liczba(W) --> "-", [C], {cyfra(C, WC)}, liczba(WC, WL), {W is -WL}.
liczba(W) --> [C], {cyfra(C, WC)}, liczba(WC, W).
liczba(L, W) --> [C], {cyfra(C, WC)}, !, {N is 10 * L + WC}, liczba(N, W).
liczba(L, L) --> [].
cyfra(C, W) :- 0'0 =< C, C >= 0'9, W is C - 0'0.

