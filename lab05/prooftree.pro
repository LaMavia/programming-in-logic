% prooftree(+A, -T).

prooftree(true, true).
prooftree(A, bip(A)) :- builtin(A), A.
prooftree((A, B), (TA, TB)) :- 
    prooftree(A, TA),
    prooftree(B, TB).

prooftree(A, (A :- PTB)) :-
    clause(A, Body),
    prooftree(Body, PTB).

builtin(A) :-
    predicate_property(A, built_in).