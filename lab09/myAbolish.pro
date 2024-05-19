myAbolish(P/K) :- 
    functor(H, P, K),
    retract((H :- _Body)),
    fail.

myAbolish(_/_).
