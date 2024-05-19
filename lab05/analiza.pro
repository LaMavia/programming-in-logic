% note: jak coś nie działa na ustalonej (false), 
% to uruchamiamy ze zmiennymi.
% note2: arguments not sufficently instantiated - szukaj przy "is".
% note3: 
% analiza(Klauzula, CzyDyrektywa, IleAtomów, ListaPredTreści),
analiza((:- op(_, R, _)), oper(R), 0, []) :- !.

analiza((:- _D), inna, 0, []) :- !.
    % functor(D, F, K),
    % F/K \= op/3.

analiza((_Head :- Body), nie, K, LP) :- 
    !, 
    auxAnaliza(Body, 0, K, LP).

analiza(Head, nie, 0, []) :-
    functor(Head, F, _K),
    F \= ':-'.



auxAnaliza((H, T), N, K, [ (P/AP) | LP ]) :-
    !, % jedyna, która obsługuje przecinek
    functor(H, P, AP),
    NNext is N + 1,
    auxAnaliza(T, NNext, K, LP).

auxAnaliza(true, K, K, []) :- !.

auxAnaliza(T, N, K, [ (P/AP) ]) :-
    functor(T, P, AP),
    P \= ',',
    K is N + 1.
    