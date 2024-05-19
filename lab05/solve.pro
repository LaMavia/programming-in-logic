% solve(+A) <=> A.
solve(true) :- !.

solve(A) :-
    builtin(A),
    A.

solve((A,B)) :- !, solve(A), solve(B).

solve(A) :- 
    clause(A, B),
    solve(B).

solve(A) :-
    builtin(A),
    A.

builtin(A) :-
    predicate_property(A, built_in).